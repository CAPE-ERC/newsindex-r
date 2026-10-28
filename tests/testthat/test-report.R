.sample_result <- function() {
  data.frame(
    country = c("Nigeria", "Nigeria", "Nigeria", "Kenya"),
    year_month = c("2020-01", "2020-02", "2020-03", "2020-01"),
    EPU = c(90, 150, 100, 100),
    M_t = c(0.9, 1.5, 1.0, 1.0),
    mu = c(1, 1, 1, 1),
    n_sites = c(1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

test_that("summarize() reports first/last/pct-change/biggest-move per country", {
  text <- summarize(.sample_result())

  expect_true(grepl("Nigeria", text, fixed = TRUE))
  expect_true(grepl("Kenya", text, fixed = TRUE))
  expect_true(grepl("90.0", text, fixed = TRUE))
  expect_true(grepl("biggest move", text, fixed = TRUE))
})

test_that("summarize() handles a single-month country without a biggest-move line", {
  text <- summarize(.sample_result()[4, , drop = FALSE])
  expect_equal(text, "Kenya: EPU = 100.0 (2020-01).")
})

test_that("print_period() filters to the inclusive window and returns it invisibly", {
  out <- testthat::capture_output(result <- print_period(.sample_result(), "2020-01", "2020-02"))
  expect_setequal(result$year_month, c("2020-01", "2020-02"))
  expect_true(nchar(out) > 0)
})

test_that("print_period() errors clearly when the window has no rows", {
  expect_error(print_period(.sample_result(), "2099-01", "2099-02"), "no rows")
})

test_that("describe() summarizes min/max/mean/std and the biggest move per country", {
  out <- describe(.sample_result())

  nigeria <- out[out$country == "Nigeria", ]
  expect_equal(nigeria$min, 90)
  expect_equal(nigeria$max, 150)
  expect_equal(nigeria$n_months, 3)
  expect_equal(nigeria$biggest_move, 60)
  expect_equal(nigeria$biggest_move_month, "2020-02")

  kenya <- out[out$country == "Kenya", ]
  expect_equal(kenya$n_months, 1)
  expect_true(is.na(kenya$biggest_move))
})

test_that("top() sorts descending by column and errors on an unknown column", {
  out <- top(.sample_result(), n = 2, by = "EPU")
  expect_equal(out$EPU, c(150, 100))

  expect_error(top(.sample_result(), by = "nope"), "not a column")
})

test_that("report() renders HTML and Markdown and rejects other extensions", {
  html_path <- tempfile(fileext = ".html")
  report(.sample_result(), html_path)
  html <- paste(readLines(html_path, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("<table", html, fixed = TRUE))
  expect_true(grepl("Nigeria", html, fixed = TRUE))

  md_path <- tempfile(fileext = ".md")
  report(.sample_result(), md_path)
  md <- paste(readLines(md_path, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("| Nigeria |", md, fixed = TRUE))

  expect_error(report(.sample_result(), tempfile(fileext = ".pdf")), "unsupported extension")
})

test_that("print.newsindex_baseline prints a readable summary", {
  baseline <- structure(
    list(
      start = "2015-01", end = "2023-12",
      mu = c(Nigeria = 1.0, Kenya = 1.0),
      sigma = list(Nigeria = c("The Punch" = 0.1), Kenya = c("Daily Nation" = 0.2)),
      dictionary_fingerprint = "", library_version = "0.2.0"
    ),
    class = "newsindex_baseline"
  )
  out <- testthat::capture_output(print(baseline))
  expect_true(grepl("2015-01..2023-12", out, fixed = TRUE))
  expect_true(grepl("Nigeria", out, fixed = TRUE))
})
