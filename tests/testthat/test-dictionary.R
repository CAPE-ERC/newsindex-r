production_dictionary_path <- system.file("extdata", "eptu_country_keywords.json", package = "newsindex")

test_that("the production dictionary path resolves", {
  expect_true(nzchar(production_dictionary_path))
})

test_that("loading the production dictionary covers all 54 countries", {
  dictionary <- dictionary_load(production_dictionary_path)
  expect_equal(length(dictionary), 54)
  nigeria <- dictionary[["Nigeria"]]
  expect_equal(nigeria$languages, "en")
  expect_true("economy" %in% nigeria$economic)
  expect_gt(length(nigeria$policy), 0)
  expect_gt(length(nigeria$uncertainty), 0)
})

test_that("the production dictionary has no empty or overlapping EPU components", {
  dictionary <- dictionary_load(production_dictionary_path)
  issues <- dictionary_validate(dictionary)
  expect_equal(length(issues), 0)
})

test_that("fingerprint is stable and order independent", {
  payload <- list(countries = list(
    B = list(languages = list("en"), ECONOMY_TERMS = list("gdp"), POLICY_TERMS = list("tax"), UNCERTAINTY_TERMS = list("risk")),
    A = list(languages = list("en"), ECONOMY_TERMS = list("gdp"), POLICY_TERMS = list("tax"), UNCERTAINTY_TERMS = list("risk"))
  ))
  reordered <- list(countries = payload$countries[c("A", "B")])

  path1 <- tempfile(fileext = ".json")
  path2 <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE), path1)
  writeLines(jsonlite::toJSON(reordered, auto_unbox = TRUE), path2)

  expect_equal(
    dictionary_fingerprint(dictionary_load(path1)),
    dictionary_fingerprint(dictionary_load(path2))
  )
})

test_that("fingerprint changes when a term changes", {
  base <- list(countries = list(
    A = list(languages = list("en"), ECONOMY_TERMS = list("gdp"), POLICY_TERMS = list("tax"), UNCERTAINTY_TERMS = list("risk"))
  ))
  changed <- base
  changed$countries$A$ECONOMY_TERMS <- list("gdp", "inflation")

  path1 <- tempfile(fileext = ".json")
  path2 <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(base, auto_unbox = TRUE), path1)
  writeLines(jsonlite::toJSON(changed, auto_unbox = TRUE), path2)

  expect_false(identical(
    dictionary_fingerprint(dictionary_load(path1)),
    dictionary_fingerprint(dictionary_load(path2))
  ))
})

test_that("validate flags empty component and cross-component overlap", {
  payload <- list(countries = list(
    A = list(
      languages = list("en"), ECONOMY_TERMS = list("gdp", "risk"),
      POLICY_TERMS = list("tax"), UNCERTAINTY_TERMS = list("risk")
    ),
    B = list(
      languages = list("en"), ECONOMY_TERMS = list(),
      POLICY_TERMS = list("tax"), UNCERTAINTY_TERMS = list("risk")
    )
  ))
  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE), path)

  issues <- dictionary_validate(dictionary_load(path))
  pairs <- lapply(issues, function(issue) c(issue$country, issue$issue))
  expect_true(any(vapply(pairs, function(p) identical(p, c("A", "cross_component_overlap")), logical(1))))
  expect_true(any(vapply(pairs, function(p) identical(p, c("B", "empty_component")), logical(1))))
})
