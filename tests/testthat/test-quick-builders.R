test_that("quick_index runs one country for one calendar year", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- quick_index(root, dict_path, "Nigeria", 2020, verbose = FALSE)

  expect_setequal(result$country, "Nigeria")
  expect_true(all(result$year_month >= "2020-01" & result$year_month <= "2020-12"))
})

test_that("index_all reads every country folder", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- index_all(root, dict_path, "2020-01", "2020-03", verbose = FALSE)

  expect_setequal(result$country, c("Nigeria", "Kenya"))
})

test_that("index_countries filters to a named list", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- index_countries(root, dict_path, c("Kenya"), "2020-01", "2020-03", verbose = FALSE)

  expect_setequal(result$country, "Kenya")
})

test_that("index_region resolves a named region to its member countries", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- index_region(
    root, dict_path, "West", "2020-01", "2020-03",
    groups = list(West = c("Nigeria"), East = c("Kenya")),
    verbose = FALSE
  )

  expect_setequal(result$country, "Nigeria")
})

test_that("index_region accepts an explicit country vector, bypassing groups", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  result <- index_region(root, dict_path, c("Nigeria", "Kenya"), "2020-01", "2020-03", verbose = FALSE)

  expect_setequal(result$country, c("Nigeria", "Kenya"))
})

test_that("index_region errors clearly on an unknown region name", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  expect_error(
    index_region(root, dict_path, "Nowhere", "2020-01", "2020-03", groups = list(West = "Nigeria"), verbose = FALSE),
    "unknown region"
  )
})

test_that(".month_window computes an inclusive months_back range without wall-clock dependence", {
  expect_equal(.month_window(12, as.Date("2024-06-15")), c("2023-07", "2024-06"))
  expect_equal(.month_window(1, as.Date("2024-01-10")), c("2024-01", "2024-01"))
  expect_equal(.month_window(13, as.Date("2024-01-10")), c("2023-01", "2024-01"))
})

test_that("index_since errors on a non-positive months_back", {
  expect_error(.month_window(0), "months_back")
})

test_that("rerun() reproduces a run_index() result from its attached config", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  original <- run_index(root, dict_path, "2020-01", "2020-03", countries = "Nigeria", verbose = FALSE)
  again <- rerun(original)

  attr(original, "newsindex_config") <- NULL
  attr(again, "newsindex_config") <- NULL
  expect_equal(original, again)
})

test_that("rerun() reproduces a run_index_with_baseline() result, reusing the frozen baseline", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  original <- run_index_with_baseline(
    root, dict_path,
    windows = list(c("2020-03", "2020-03")),
    baseline_start = "2020-01", baseline_end = "2020-02",
    verbose = FALSE
  )
  again <- rerun(original$result)

  expect_identical(again$baseline, original$baseline)
  attr(original$result, "newsindex_config") <- NULL
  attr(again$result, "newsindex_config") <- NULL
  expect_equal(original$result, again$result)
})

test_that("rerun() also accepts the whole run_index_with_baseline() return list", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  original <- run_index_with_baseline(
    root, dict_path,
    windows = list(c("2020-03", "2020-03")),
    baseline_start = "2020-01", baseline_end = "2020-02",
    verbose = FALSE
  )
  again <- rerun(original)

  expect_identical(again$baseline, original$baseline)
})

test_that("rerun() errors clearly when no config is attached or given", {
  plain <- data.frame(country = "Nigeria", EPU = 100)
  expect_error(rerun(plain), "newsindex_config")
  expect_error(rerun(list(not_fn = 1)), "newsindex_config")
})

test_that("rerun() overrides the stored verbose value when given one", {
  root <- tempfile()
  dir.create(root)
  .build_pipeline_archive(root)
  dict_path <- .write_pipeline_dictionary()

  original <- run_index(root, dict_path, "2020-01", "2020-03", countries = "Nigeria", verbose = FALSE)
  msgs <- testthat::capture_messages(rerun(original, verbose = TRUE))
  expect_true(length(msgs) > 0)
})
