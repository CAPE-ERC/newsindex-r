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
  expect_equal(unique(articles$year_month), "2026-03")
  row <- articles[articles$country == "Nigeria", ]
  expect_equal(row$title, "Government’s policy shift")
})

test_that("read_articles errors when nothing is found", {
  root <- tempfile()
  dir.create(root)
  expect_error(read_articles(root), "no article files found")
})
