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
