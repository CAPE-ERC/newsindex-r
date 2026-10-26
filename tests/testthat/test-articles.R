.write_articles <- function(path, rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- paste(c("site,date,title,url,text", rows), collapse = "\n")
  con <- gzfile(path, "w")
  writeLines(body, con)
  close(con)
}

test_that("read_articles walks the country/newspaper/month layout and unescapes HTML", {
  root <- tempfile()
  dir.create(root)

  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-03", "articles_The Punch_2026-03.csv.gz"),
    c('The Punch,2026-03-01,"Government&#8217;s policy shift",https://example.com/a,"Full text"')
  )
  .write_articles(
    file.path(root, "Kenya", "Daily Nation", "2026-03", "articles_Daily Nation_2026-03.csv.gz"),
    c("Daily Nation,2026-03-02,Economy grows,https://example.com/b,More text")
  )

  articles <- read_articles(root)

  expect_equal(nrow(articles), 2)
  expect_setequal(articles$country, c("Nigeria", "Kenya"))
  expect_equal(as.character(unique(articles$year_month)), "2026-03")
  row <- articles[articles$country == "Nigeria", ]
  expect_equal(row$title, "Government’s policy shift")
})

test_that("read_articles returns factor country/site/year_month", {
  root <- tempfile()
  dir.create(root)
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-03", "articles_The Punch_2026-03.csv.gz"),
    c("The Punch,2026-03-01,Headline,https://example.com/a,Text")
  )

  articles <- read_articles(root)

  for (col in c("country", "site", "year_month")) {
    expect_true(is.factor(articles[[col]]))
  }
})

test_that("read_articles errors when nothing is found", {
  root <- tempfile()
  dir.create(root)
  expect_error(read_articles(root), "no article files found")
})

test_that("read_articles countries filters to the named folders (case-insensitively)", {
  root <- tempfile()
  dir.create(root)
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-03", "articles_The Punch_2026-03.csv.gz"),
    c("The Punch,2026-03-01,Headline,https://example.com/a,Text")
  )
  .write_articles(
    file.path(root, "Kenya", "Daily Nation", "2026-03", "articles_Daily Nation_2026-03.csv.gz"),
    c("Daily Nation,2026-03-02,Economy grows,https://example.com/b,More text")
  )

  articles <- read_articles(root, countries = "nigeria")
  expect_setequal(articles$country, "Nigeria")
})

test_that("read_articles countries 'all' reads every folder", {
  root <- tempfile()
  dir.create(root)
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-03", "articles_The Punch_2026-03.csv.gz"),
    c("The Punch,2026-03-01,Headline,https://example.com/a,Text")
  )
  .write_articles(
    file.path(root, "Kenya", "Daily Nation", "2026-03", "articles_Daily Nation_2026-03.csv.gz"),
    c("Daily Nation,2026-03-02,Economy grows,https://example.com/b,More text")
  )

  articles <- read_articles(root, countries = "all")
  expect_setequal(articles$country, c("Nigeria", "Kenya"))
})

test_that("read_articles countries errors on an unknown folder", {
  root <- tempfile()
  dir.create(root)
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-03", "articles_The Punch_2026-03.csv.gz"),
    c("The Punch,2026-03-01,Headline,https://example.com/a,Text")
  )

  expect_error(read_articles(root, countries = c("Nigeria", "Ghana")), "Ghana")
})

.build_multi_month_archive <- function(root) {
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-01", "articles_The Punch_2026-01.csv.gz"),
    "The Punch,2026-01-05,Jan headline,https://example.com/a,Text A"
  )
  .write_articles(
    file.path(root, "Nigeria", "Vanguard", "2026-01", "articles_Vanguard_2026-01.csv.gz"),
    "Vanguard,2026-01-06,Jan headline 2,https://example.com/b,Text B"
  )
  .write_articles(
    file.path(root, "Nigeria", "The Punch", "2026-02", "articles_The Punch_2026-02.csv.gz"),
    "The Punch,2026-02-10,Feb headline,https://example.com/c,Text C"
  )
  .write_articles(
    file.path(root, "Kenya", "Daily Nation", "2026-01", "articles_Daily Nation_2026-01.csv.gz"),
    "Daily Nation,2026-01-07,Kenya headline,https://example.com/d,Text D"
  )
}

test_that("country_month_batches yields one batch per country-month", {
  root <- tempfile()
  dir.create(root)
  .build_multi_month_archive(root)

  batches <- country_month_batches(root, function(b) b)

  # Nigeria/2026-01 (2 newspapers combined), Nigeria/2026-02, Kenya/2026-01
  expect_equal(length(batches), 3)
  sizes <- sort(vapply(batches, nrow, integer(1)))
  expect_equal(sizes, c(1, 1, 2))
  for (batch in batches) {
    expect_equal(length(unique(as.character(batch$country))), 1)
  }
})

test_that("country_month_batches matches read_articles when combined", {
  root <- tempfile()
  dir.create(root)
  .build_multi_month_archive(root)

  streamed <- do.call(rbind, country_month_batches(root, function(b) b))
  whole <- read_articles(root)

  sort_cols <- c("country", "site", "date", "url")
  streamed <- streamed[do.call(order, lapply(sort_cols, function(c) as.character(streamed[[c]]))), ]
  whole <- whole[do.call(order, lapply(sort_cols, function(c) as.character(whole[[c]]))), ]
  for (col in names(streamed)) {
    expect_equal(as.character(streamed[[col]]), as.character(whole[[col]]))
  }
})

test_that("country_month_batches countries filters", {
  root <- tempfile()
  dir.create(root)
  .build_multi_month_archive(root)

  batches <- country_month_batches(root, function(b) b, countries = "nigeria")

  expect_true(all(vapply(batches, function(b) all(as.character(b$country) == "Nigeria"), logical(1))))
  expect_equal(sum(vapply(batches, nrow, integer(1))), 3)
})

test_that("country_month_batches errors when nothing is found", {
  root <- tempfile()
  dir.create(root)
  expect_error(country_month_batches(root, function(b) b), "no article files found")
})
