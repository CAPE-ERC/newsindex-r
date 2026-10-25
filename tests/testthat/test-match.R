test_that("normalize_text folds case, apostrophes, and dashes", {
  expect_equal(normalize_text("Government’s  POLICY—shift"), "government's policy-shift")
})

test_that("normalize_text handles NULL/NA and non-strings", {
  expect_equal(normalize_text(NULL), "")
  expect_equal(normalize_text(NA), "")
  expect_equal(normalize_text(42), "42")
})

test_that("short abbreviations do not match inside words", {
  expect_false(any_term_matches("Each country acts", c("EAC")))
  expect_false(any_term_matches("The newcomer arrived", c("OMC")))
  expect_false(any_term_matches("A neural network", c("EU")))
})

test_that("complete abbreviations and phrases match", {
  expect_true(any_term_matches("An EAC trade agreement", c("EAC")))
  expect_true(any_term_matches("free-trade agreement signed", c("free trade agreement")))
  expect_true(any_term_matches("free trade agreement signed", c("free-trade agreement")))
})

test_that("Arabic and Portuguese terms match with word boundaries", {
  expect_true(any_term_matches(
    "أزمة اقتصادية مع تغييرات في السياسة التجارية",
    c("أزمة اقتصادية")
  ))
  expect_true(any_term_matches("incerteza economica", c("incerteza")))
  expect_false(any_term_matches("incertezabilidade", c("incerteza")))
})

test_that("find_terms returns distinct matches in order, no duplicates", {
  terms <- c("economy", "inflation", "policy", "economy")
  found <- find_terms("The economy faces inflation amid new policy and economy news", terms)
  expect_equal(found, c("economy", "inflation", "policy"))
})

test_that("is_epu requires all three components", {
  economic <- c("economy", "inflation")
  policy <- c("government", "fiscal policy")
  uncertainty <- c("uncertainty", "volatile")

  expect_true(is_epu("Government policy creates uncertainty", "in the economy", economic, policy, uncertainty))
  expect_false(is_epu("The economy is growing", "", economic, policy, uncertainty))
  expect_false(is_epu("Uncertainty remains after the football match", "", economic, policy, uncertainty))
})
