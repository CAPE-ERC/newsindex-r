test_that("run_index computes a single-window index", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- run_index(root, dict_path, "2020-01", "2020-03", verbose = FALSE)

  expect_true(all(result$country %in% c("Nigeria", "Kenya")))
  expect_true(all(result$EPU >= 0))
})

test_that("run_index prints progress by default", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  msgs <- testthat::capture_messages(run_index(root, dict_path, "2020-01", "2020-03"))
  expect_true(length(msgs) > 0)
})

test_that("run_index countries filters the archive", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- run_index(root, dict_path, "2020-01", "2020-03", countries = "Nigeria", verbose = FALSE)

  expect_setequal(result$country, "Nigeria")
})

test_that("run_index errors clearly on a missing dictionary file", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)

  expect_error(
    run_index(root, file.path(root, "no-such-file.json"), "2020-01", "2020-03", verbose = FALSE),
    "no-such-file.json"
  )
})

test_that("run_index_with_baseline fits and scores in one call", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  out <- run_index_with_baseline(
    root, dict_path,
    windows = list(c("2020-03", "2020-03")),
    baseline_start = "2020-01", baseline_end = "2020-02",
    verbose = FALSE
  )

  expect_equal(out$baseline$start, "2020-01")
  expect_equal(out$baseline$end, "2020-02")
  expect_setequal(out$result$window, "2020-03:2020-03")
  expect_true(all(out$result$country %in% c("Nigeria", "Kenya")))
})

test_that("run_index_with_baseline accepts an already-fitted baseline", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  kw <- dictionary_load(dict_path)
  counts <- count_articles(dedupe_articles(read_articles(root)), kw)
  fitted <- fit_baseline(counts, "2020-01", "2020-02")

  out <- run_index_with_baseline(
    root, dict_path,
    windows = list(c("2020-03", "2020-03")),
    baseline = fitted,
    verbose = FALSE
  )

  expect_identical(out$baseline, fitted)
  expect_setequal(out$result$window, "2020-03:2020-03")
})

test_that("run_index_with_baseline requires a baseline or a window", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  expect_error(
    run_index_with_baseline(root, dict_path, windows = list(c("2020-03", "2020-03")), verbose = FALSE),
    "baseline"
  )
})

test_that("run_index_with_baseline rejects conflicting baseline arguments", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  kw <- dictionary_load(dict_path)
  counts <- count_articles(dedupe_articles(read_articles(root)), kw)
  fitted <- fit_baseline(counts, "2020-01", "2020-02")

  expect_error(
    run_index_with_baseline(
      root, dict_path,
      windows = list(c("2020-03", "2020-03")),
      baseline = fitted, baseline_start = "2020-01",
      verbose = FALSE
    ),
    "baseline_start"
  )
})
