test_that("countries covers the full ISO 3166-1 list", {
  table <- countries()
  expect_equal(nrow(table), 249)
  expect_equal(
    names(table),
    c("name", "iso2", "iso3", "continent", "sub_region", "intermediate_region")
  )
})

test_that("countries codes are unique and well-formed", {
  table <- countries()
  expect_true(all(nchar(table$iso2) == 2))
  expect_true(all(nchar(table$iso3) == 3))
  expect_equal(length(unique(table$iso2)), nrow(table))
  expect_equal(length(unique(table$iso3)), nrow(table))
})

test_that("countries does not treat Namibia's iso2 code 'NA' as a missing value", {
  table <- countries()
  rownames(table) <- table$iso3
  expect_identical(table["NAM", "iso2"], "NA")
  expect_false(is.na(table["NAM", "iso2"]))
})

test_that("countries spot-checks known entries", {
  table <- countries()
  rownames(table) <- table$iso3

  expect_equal(table["NGA", "name"], "Nigeria")
  expect_equal(table["NGA", "iso2"], "NG")
  expect_equal(table["NGA", "continent"], "Africa")
  expect_equal(table["NGA", "sub_region"], "Sub-Saharan Africa")
  expect_equal(table["NGA", "intermediate_region"], "Western Africa")

  expect_equal(table["FRA", "continent"], "Europe")
  expect_equal(table["FRA", "sub_region"], "Western Europe")
  # Europe has no third regional level.
  expect_equal(table["FRA", "intermediate_region"], "")
})

test_that("countries has blank classification for entries outside UN M49", {
  table <- countries()
  rownames(table) <- table$iso3
  expect_equal(table["ATA", "continent"], "")
  expect_equal(table["ATA", "sub_region"], "")
})

test_that("countries: only Africa and the Americas have an intermediate_region", {
  table <- countries()
  with_intermediate <- table[table$intermediate_region != "", ]
  expect_setequal(unique(with_intermediate$continent), c("Africa", "Americas"))
})

test_that("countries accepts a path override", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "name,iso2,iso3,continent,sub_region,intermediate_region",
      "Freedonia,FD,FRD,Europe,Western Europe,"
    ),
    path
  )
  table <- countries(path)
  expect_equal(nrow(table), 1)
  expect_equal(table$name[1], "Freedonia")
})
