test_that("count_articles counts EPU and total articles per site-month", {
  dict_path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(countries = list(
    Freedonia = list(
      languages = list("en"), ECONOMY_TERMS = list("economy"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertainty")
    )
  )), auto_unbox = TRUE), dict_path)
  kw <- dictionary_load(dict_path)

  articles <- data.frame(
    country = c("Freedonia", "Freedonia", "Freedonia"),
    site = c("SiteA", "SiteA", "SiteA"),
    year_month = c("2020-01", "2020-01", "2020-01"),
    title = c(
      "Government economy uncertainty rises",
      "Economy grows steadily",
      "Sports results this week"
    ),
    text = c("", "", ""),
    stringsAsFactors = FALSE
  )
  counts <- count_articles(articles, kw)
  expect_equal(counts$epu_articles[1], 1)
  expect_equal(counts$all_articles[1], 3)
})

test_that("count_articles errors on a country missing from the dictionary", {
  dict_path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(countries = list(
    Freedonia = list(
      languages = list("en"), ECONOMY_TERMS = list("economy"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertainty")
    )
  )), auto_unbox = TRUE), dict_path)
  kw <- dictionary_load(dict_path)

  articles <- data.frame(
    country = "Sylvania", site = "SiteA", year_month = "2020-01",
    title = "x", text = "y", stringsAsFactors = FALSE
  )
  expect_error(count_articles(articles, kw), "Sylvania")
})

.hand_computed_counts <- function() {
  rows <- list()
  # SiteA: S_it alternates 0.1, 0.3 -> population std = 0.1, retained.
  months <- c("2020-01", "2020-02", "2020-03", "2020-04")
  epu <- c(1, 3, 1, 3)
  for (i in seq_along(months)) {
    rows[[length(rows) + 1]] <- data.frame(
      country = "Freedonia", site = "SiteA", year_month = months[i],
      epu_articles = epu[i], all_articles = 10, stringsAsFactors = FALSE
    )
  }
  # SiteB: constant S_it = 0.25 -> zero variance, dropped.
  for (month in months) {
    rows[[length(rows) + 1]] <- data.frame(
      country = "Freedonia", site = "SiteB", year_month = month,
      epu_articles = 2, all_articles = 8, stringsAsFactors = FALSE
    )
  }
  # SiteC: a single baseline month -> too few observations, dropped.
  rows[[length(rows) + 1]] <- data.frame(
    country = "Freedonia", site = "SiteC", year_month = "2020-01",
    epu_articles = 1, all_articles = 4, stringsAsFactors = FALSE
  )
  # SiteA continues into a post-baseline month.
  rows[[length(rows) + 1]] <- data.frame(
    country = "Freedonia", site = "SiteA", year_month = "2020-05",
    epu_articles = 3, all_articles = 10, stringsAsFactors = FALSE
  )
  # SiteB also has a post-baseline month; it must not affect scoring since
  # it was dropped at fit_baseline time.
  rows[[length(rows) + 1]] <- data.frame(
    country = "Freedonia", site = "SiteB", year_month = "2020-05",
    epu_articles = 4, all_articles = 8, stringsAsFactors = FALSE
  )
  do.call(rbind, rows)
}

test_that("fit_baseline drops zero-variance and single-observation sites", {
  counts <- .hand_computed_counts()
  baseline <- fit_baseline(counts, "2020-01", "2020-04")

  expect_equal(baseline_retained_sites(baseline, "Freedonia"), "SiteA")
  expect_equal(unname(baseline$sigma[["Freedonia"]]["SiteA"]), 0.1)
  expect_equal(unname(baseline$mu["Freedonia"]), 2.0)
})

test_that("score_index matches hand-computed EPU and freezes dropped sites", {
  counts <- .hand_computed_counts()
  baseline <- fit_baseline(counts, "2020-01", "2020-04")
  scored <- score_index(counts, baseline)
  rownames(scored) <- scored$year_month

  expect_equal(scored["2020-01", "EPU"], 50.0)
  expect_equal(scored["2020-02", "EPU"], 150.0)
  expect_equal(scored["2020-01", "n_sites"], 1L)

  # Post-baseline month: SiteA S=0.3 -> N=3 -> EPU=150; SiteB is ignored
  # entirely even though it has 2020-05 data, because it was dropped at
  # fit_baseline time.
  expect_equal(scored["2020-05", "EPU"], 150.0)
  expect_equal(scored["2020-05", "n_sites"], 1L)
})

test_that("compute_index is an in-sample fit and score for one window", {
  counts <- .hand_computed_counts()
  result <- compute_index(counts, "2020-01", "2020-04")
  rownames(result) <- result$year_month

  # Same numbers as fit_baseline+score over the identical window: sigma and
  # mu are fit from (and only from) this one window, then it is scored
  # against itself -- nothing is frozen or reused.
  expect_equal(result["2020-01", "EPU"], 50.0)
  expect_equal(result["2020-02", "EPU"], 150.0)
  # The post-baseline 2020-05 row exists in counts but is outside this
  # single window, so it must not appear in the result.
  expect_false("2020-05" %in% result$year_month)
})

test_that("score_windows scores multiple named periods against one frozen baseline", {
  counts <- rbind(
    .hand_computed_counts(),
    data.frame(
      country = "Freedonia", site = "SiteA", year_month = "2020-06",
      epu_articles = 1, all_articles = 10, stringsAsFactors = FALSE
    )
  )
  baseline <- fit_baseline(counts, "2020-01", "2020-04")

  result <- score_windows(counts, baseline, list(c("2020-05", "2020-05"), c("2020-06", "2020-06")))
  rownames(result) <- paste(result$window, result$year_month)

  expect_equal(result["2020-05:2020-05 2020-05", "EPU"], 150.0)
  expect_equal(result["2020-06:2020-06 2020-06", "EPU"], 50.0)
  expect_setequal(unique(result$window), c("2020-05:2020-05", "2020-06:2020-06"))
})

test_that("score_windows with no windows returns an empty frame with expected columns", {
  counts <- .hand_computed_counts()
  baseline <- fit_baseline(counts, "2020-01", "2020-04")
  result <- score_windows(counts, baseline, list())
  expect_equal(names(result), c("window", "country", "year_month", "EPU", "M_t", "mu", "n_sites"))
  expect_equal(nrow(result), 0)
})

test_that("baseline_save and baseline_load round-trip", {
  counts <- .hand_computed_counts()
  baseline <- fit_baseline(counts, "2020-01", "2020-04", dictionary_fingerprint = "abc123")
  path <- tempfile(fileext = ".json")
  baseline_save(baseline, path)
  reloaded <- baseline_load(path)

  expect_equal(reloaded$mu, baseline$mu)
  expect_equal(reloaded$sigma, baseline$sigma)
  expect_equal(reloaded$dictionary_fingerprint, "abc123")
  expect_equal(unname(reloaded$mu["Freedonia"]), 2.0)
})

test_that("baseline_save does not lose precision on values needing many digits", {
  # A regression test for a real bug: jsonlite::toJSON() truncates to 4
  # significant digits by default, which silently corrupted a sigma of
  # 1/6 = 0.1666... to 0.1667 on a save/load round-trip.
  counts <- data.frame(
    country = "Freedonia", site = rep(c("SiteA", "SiteB"), each = 3),
    year_month = rep(c("2020-01", "2020-02", "2020-03"), 2),
    epu_articles = c(1, 2, 1, 1, 2, 1), all_articles = c(3, 3, 3, 3, 3, 4),
    stringsAsFactors = FALSE
  )
  baseline <- fit_baseline(counts, "2020-01", "2020-03")
  path <- tempfile(fileext = ".json")
  baseline_save(baseline, path)
  reloaded <- baseline_load(path)

  expect_equal(reloaded$sigma, baseline$sigma)
  expect_equal(reloaded$mu, baseline$mu)
})

.write_count_archive_dictionary <- function(path) {
  writeLines(jsonlite::toJSON(list(countries = list(
    Nigeria = list(
      languages = list("en"), ECONOMY_TERMS = list("fiscal", "inflation"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertainty")
    ),
    Kenya = list(
      languages = list("en"), ECONOMY_TERMS = list("fiscal", "inflation"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertain")
    )
  )), auto_unbox = TRUE), path)
}

.write_count_archive_articles <- function(path, rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- paste(c("site,date,title,url,text", rows), collapse = "\n")
  con <- gzfile(path, "w")
  writeLines(body, con)
  close(con)
}

.build_count_archive <- function(root) {
  .write_count_archive_articles(
    file.path(root, "Nigeria", "The Punch", "2020-01", "articles_The Punch_2020-01.csv.gz"),
    c(
      'The Punch,2020-01-01,"Government fiscal policy creates uncertainty",https://x/n1,body',
      'The Punch,2020-01-02,"Government fiscal policy creates uncertainty",https://x/n1,body',
      "The Punch,2020-01-03,Local football result,https://x/n2,body"
    )
  )
  .write_count_archive_articles(
    file.path(root, "Nigeria", "Vanguard", "2020-01", "articles_Vanguard_2020-01.csv.gz"),
    'Vanguard,2020-01-04,"Inflation and government uncertainty rise",https://x/n3,body'
  )
  .write_count_archive_articles(
    file.path(root, "Nigeria", "The Punch", "2020-02", "articles_The Punch_2020-02.csv.gz"),
    "The Punch,2020-02-01,Weather update,https://x/n4,body"
  )
  .write_count_archive_articles(
    file.path(root, "Kenya", "Daily Nation", "2020-01", "articles_Daily Nation_2020-01.csv.gz"),
    'Daily Nation,2020-01-05,"Government fiscal plan uncertain economy",https://x/k1,body'
  )
}

test_that("count_archive matches count_articles(dedupe_articles(read_articles(...)))", {
  root <- tempfile()
  dir.create(root)
  .build_count_archive(root)
  dict_path <- tempfile(fileext = ".json")
  .write_count_archive_dictionary(dict_path)
  kw <- dictionary_load(dict_path)

  streamed <- count_archive(root, kw)
  whole <- count_articles(dedupe_articles(read_articles(root)), kw)

  sort_cols <- c("country", "site", "year_month")
  streamed <- streamed[do.call(order, streamed[sort_cols]), ]
  whole <- whole[do.call(order, whole[sort_cols]), ]
  rownames(streamed) <- NULL
  rownames(whole) <- NULL
  expect_equal(streamed, whole)
})

test_that("count_archive dedupe = FALSE counts duplicates separately", {
  root <- tempfile()
  dir.create(root)
  .build_count_archive(root)
  dict_path <- tempfile(fileext = ".json")
  .write_count_archive_dictionary(dict_path)
  kw <- dictionary_load(dict_path)

  deduped <- count_archive(root, kw, dedupe = TRUE)
  raw <- count_archive(root, kw, dedupe = FALSE)

  punch_deduped <- deduped[deduped$country == "Nigeria" & deduped$site == "The Punch", ]
  punch_raw <- raw[raw$country == "Nigeria" & raw$site == "The Punch", ]
  expect_equal(sum(punch_raw$all_articles), sum(punch_deduped$all_articles) + 1)
})

test_that("count_archive countries filters", {
  root <- tempfile()
  dir.create(root)
  .build_count_archive(root)
  dict_path <- tempfile(fileext = ".json")
  .write_count_archive_dictionary(dict_path)
  kw <- dictionary_load(dict_path)

  result <- count_archive(root, kw, countries = "nigeria")

  expect_setequal(result$country, "Nigeria")
})

test_that("count_archive calls progress once per batch", {
  root <- tempfile()
  dir.create(root)
  .build_count_archive(root)
  dict_path <- tempfile(fileext = ".json")
  .write_count_archive_dictionary(dict_path)
  kw <- dictionary_load(dict_path)

  messages <- character(0)
  count_archive(root, kw, progress = function(m) messages[[length(messages) + 1]] <<- m)

  # Nigeria/2020-01 (2 newspapers), Nigeria/2020-02, Kenya/2020-01
  expect_equal(length(messages), 3)
  expect_true(any(grepl("Nigeria", messages) & grepl("2020-01", messages)))
})
