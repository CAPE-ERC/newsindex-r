test_that("monthly_gdp log-linearly interpolates and holds the last year flat", {
  csv_path <- tempfile(fileext = ".csv")
  writeLines(c("Country,Year,GDP_Billions_USD", "Freedonia,2020,100", "Freedonia,2021,200"), csv_path)
  monthly <- monthly_gdp(csv_path)
  rownames(monthly) <- monthly$year_month

  expect_equal(monthly["2020-01", "gdp"], 100.0)
  # Midyear (July, month index 6) is the geometric mean of the two years.
  expect_equal(monthly["2020-07", "gdp"], sqrt(100 * 200))
  expect_equal(monthly["2021-01", "gdp"], 200.0)
  expect_equal(monthly["2021-12", "gdp"], 200.0)
})

test_that("monthly_gdp carries forward past end", {
  csv_path <- tempfile(fileext = ".csv")
  writeLines(c("Country,Year,GDP_Billions_USD", "Freedonia,2020,100"), csv_path)
  monthly <- monthly_gdp(csv_path, end = "2022-06")
  rownames(monthly) <- monthly$year_month

  expect_equal(monthly["2021-06", "gdp"], 100.0)
  expect_equal(monthly["2022-06", "gdp"], 100.0)
  expect_false("2022-07" %in% monthly$year_month)
})

test_that("monthly_gdp applies default country aliases", {
  csv_path <- tempfile(fileext = ".csv")
  writeLines(c("Country,Year,GDP_Billions_USD", "Cabo Verde,2020,10", "Burkina Faso,2020,5"), csv_path)
  monthly <- monthly_gdp(csv_path)
  expect_setequal(unique(monthly$country), c("Cape Verde", "Burkina-Faso"))
})

.country_index <- function() {
  months <- c("2020-01", "2020-02", "2020-03", "2020-04")
  data.frame(
    country = c(rep("X", 4), rep("Y", 4)),
    year_month = c(months, months),
    EPU = c(100, 110, 90, 100, 100, 90, 110, 100),
    stringsAsFactors = FALSE
  )
}

.gdp <- function() {
  months <- c("2020-01", "2020-02", "2020-03", "2020-04")
  data.frame(
    country = c(rep("X", 4), rep("Y", 4)),
    year_month = c(months, months),
    gdp = c(rep(60.0, 4), rep(40.0, 4)),
    stringsAsFactors = FALSE
  )
}

test_that("aggregate_regions matches a hand-computed weighted z-score", {
  groups <- list(TestRegion = c("X", "Y"))
  result <- aggregate_regions(.country_index(), .gdp(), groups,
    baseline_start = "2020-01", baseline_end = "2020-04"
  )
  region <- result[result$group == "TestRegion", ]
  rownames(region) <- region$year_month

  # z_X and z_Y both have baseline std sqrt(50); weighted z = 0.2 * z_X,t
  # (since z_Y,t = -z_X,t here), so the weighted series is
  # [0, 0.2*sqrt(2), -0.2*sqrt(2), 0] with population std 0.2.
  expect_equal(region["2020-01", "z"], 0.0, tolerance = 1e-9)
  expect_equal(region["2020-02", "z"], 0.2 * sqrt(2))
  expect_equal(region["2020-03", "z"], -0.2 * sqrt(2))

  expect_equal(region["2020-01", "index"], 100.0)
  expect_equal(region["2020-02", "index"], 100.0 + 50.0 * 0.2 * sqrt(2))
  expect_equal(region["2020-02", "n_countries"], 2L)
  expect_equal(region["2020-02", "weight_covered"], 100.0)
})

test_that("aggregate_regions' Africa group defaults to the union of all regions", {
  groups <- list(TestRegion = c("X", "Y"))
  result <- aggregate_regions(.country_index(), .gdp(), groups,
    baseline_start = "2020-01", baseline_end = "2020-04"
  )
  expect_true("Africa" %in% result$group)
  africa <- result[result$group == "Africa", ]
  region <- result[result$group == "TestRegion", ]
  expect_equal(africa$index[order(africa$year_month)], region$index[order(region$year_month)])
})

test_that("aggregate_regions can disable the Africa group", {
  groups <- list(TestRegion = c("X", "Y"))
  result <- aggregate_regions(.country_index(), .gdp(), groups,
    baseline_start = "2020-01", baseline_end = "2020-04", include_africa = FALSE
  )
  expect_false("Africa" %in% result$group)
})

test_that("aggregate_regions accepts a table with a custom weight column", {
  groups <- list(TestRegion = c("X", "Y"))
  population <- .gdp()
  names(population)[names(population) == "gdp"] <- "population"
  result <- aggregate_regions(.country_index(), population, groups,
    weight_col = "population", baseline_start = "2020-01", baseline_end = "2020-04"
  )
  region <- result[result$group == "TestRegion", ]
  rownames(region) <- region$year_month
  expect_equal(region["2020-02", "index"], 100.0 + 50.0 * 0.2 * sqrt(2))
})

test_that("aggregate_regions accepts a callable weighting function", {
  groups <- list(TestRegion = c("X", "Y"))
  only_x <- function(month_rows, group_name) ifelse(month_rows$country == "X", 1.0, 0.0)

  result <- aggregate_regions(.country_index(), only_x, groups,
    baseline_start = "2020-01", baseline_end = "2020-04"
  )
  region <- result[result$group == "TestRegion", ]
  rownames(region) <- region$year_month

  # Weighting X at 1 and Y at 0 reduces the group series to X's own
  # z-score exactly (baseline std sqrt(50)).
  expect_equal(region["2020-01", "z"], 0.0, tolerance = 1e-9)
  expect_equal(region["2020-02", "z"], sqrt(2))
  expect_equal(region["2020-03", "z"], -sqrt(2))
  expect_equal(region["2020-02", "weight_covered"], 1.0)
})

test_that("monthly_gdp accepts an in-memory data.frame", {
  table <- data.frame(
    Country = c("Freedonia", "Freedonia"), Year = c(2020, 2021),
    GDP_Billions_USD = c(100, 200), stringsAsFactors = FALSE
  )
  monthly <- monthly_gdp(table)
  rownames(monthly) <- monthly$year_month
  expect_equal(monthly["2020-01", "gdp"], 100.0)
  expect_equal(monthly["2021-01", "gdp"], 200.0)
})
