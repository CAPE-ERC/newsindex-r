test_that("canonical_url normalizes case, tracking params, and trailing slash", {
  a <- canonical_url("HTTPS://Example.COM/News/Story/?utm_source=x&id=1")
  b <- canonical_url("https://example.com/News/Story?id=1")
  expect_equal(a, b)
})

test_that("canonical_url handles missing URL", {
  expect_equal(canonical_url(NULL), "")
  expect_equal(canonical_url(NA), "")
  expect_equal(canonical_url("   "), "")
})

test_that("dedupe_articles collapses shared canonical URL within group", {
  articles <- data.frame(
    country = c("Nigeria", "Nigeria", "Nigeria"),
    year_month = c("2026-03", "2026-03", "2026-03"),
    url = c(
      "https://example.com/a?utm_source=twitter",
      "https://example.com/a",
      "https://example.com/b"
    ),
    title = c("A", "A (reshared)", "B"),
    text = c("text a", "text a", "text b"),
    stringsAsFactors = FALSE
  )
  result <- dedupe_articles(articles)
  expect_equal(nrow(result), 2)
  expect_setequal(result$title, c("A", "B"))
})

test_that("dedupe_articles falls back to title/text hash when URL is missing", {
  articles <- data.frame(
    country = c("Kenya", "Kenya"),
    year_month = c("2026-01", "2026-01"),
    url = c(NA, ""),
    title = c("Same story", "Same story"),
    text = c("identical body", "identical body"),
    stringsAsFactors = FALSE
  )
  result <- dedupe_articles(articles)
  expect_equal(nrow(result), 1)
})

test_that("dedupe_articles does not merge across a month boundary", {
  articles <- data.frame(
    country = c("Kenya", "Kenya"),
    year_month = c("2026-01", "2026-02"),
    url = c("https://example.com/a", "https://example.com/a"),
    title = c("A", "A"),
    text = c("text", "text"),
    stringsAsFactors = FALSE
  )
  result <- dedupe_articles(articles)
  expect_equal(nrow(result), 2)
})
