#' Unicode-aware literal term matching and EPU classification
#'
#' Terms are matched as complete lexical units, not arbitrary substrings.
#' This prevents abbreviations such as EAC, EU, and OMC from matching inside
#' longer words, while whole phrases, apostrophes, hyphens, Arabic, and
#' Ethiopic text are matched correctly. Word boundaries are determined from
#' the Unicode general category of each character (letters, numbers, and
#' combining marks count as word characters), so the same logic works for
#' scripts that do not use ASCII word-boundary conventions.
#'
#' @name match
NULL

.apostrophe_pattern <- "[\u2019\u2018`\u00b4]"
.dash_pattern <- "[\u2010\u2011\u2012\u2013\u2014\u2212]"

#' Normalize text for matching
#'
#' Applies Unicode NFKC normalization, folds apostrophe and dash variants to
#' a single form, collapses whitespace, and case-folds the result. Used for
#' both dictionary terms and article content so the two are compared on
#' equal footing.
#'
#' @param value A character scalar (or value coercible to one); `NA`/`NULL`
#'   is treated as an empty string.
#' @return A normalized character scalar.
#' @export
normalize_text <- function(value) {
  text <- if (is.null(value) || (length(value) == 1 && is.na(value))) "" else as.character(value)
  text <- stringi::stri_trans_nfkc(text)
  text <- stringi::stri_replace_all_regex(text, .apostrophe_pattern, "'")
  text <- stringi::stri_replace_all_regex(text, .dash_pattern, "-")
  text <- stringi::stri_replace_all_regex(text, "\\s+", " ")
  text <- stringi::stri_trans_nfkc_casefold(text)
  trimws(text)
}

.is_word_character <- function(character) {
  if (is.na(character) || !nzchar(character)) {
    return(FALSE)
  }
  stringi::stri_detect_regex(character, "^[\\p{L}\\p{N}\\p{M}]$")
}

.term_pattern_cache <- new.env(parent = emptyenv())

.compiled_term_pattern <- function(term) {
  normalized <- normalize_text(term)
  cached <- .term_pattern_cache[[normalized]]
  if (!is.null(cached)) {
    return(cached)
  }
  if (!nzchar(normalized)) {
    pattern <- "(?!x)x"
  } else {
    pieces <- stringi::stri_split_regex(normalized, "[\\s-]+")[[1]]
    pieces <- pieces[nzchar(pieces)]
    escaped <- vapply(pieces, stringi::stri_replace_all_regex,
      character(1),
      pattern = "([.^$|()\\[\\]{}*+?\\\\-])", replacement = "\\\\$1"
    )
    body <- paste(escaped, collapse = "(?:-|\\s)+")
    first_char <- stringi::stri_sub(normalized, 1, 1)
    last_char <- stringi::stri_sub(normalized, -1, -1)
    left <- if (.is_word_character(first_char)) "(?<![\\p{L}\\p{N}\\p{M}_])" else ""
    right <- if (.is_word_character(last_char)) "(?![\\p{L}\\p{N}\\p{M}_])" else ""
    pattern <- paste0(left, body, right)
  }
  .term_pattern_cache[[normalized]] <- pattern
  pattern
}

#' Does one term match already-normalized text?
#'
#' @param normalized_text Text already returned by [normalize_text()].
#' @param term A single dictionary term (not yet normalized).
#' @return `TRUE`/`FALSE`.
#' @export
term_matches <- function(normalized_text, term) {
  stringi::stri_detect_regex(normalized_text, .compiled_term_pattern(term))
}

#' Every distinct dictionary term found in text
#'
#' @param text Article title or body (or title + body).
#' @param terms Character vector of dictionary terms.
#' @return A character vector of the distinct terms found, in `terms` order.
#' @export
find_terms <- function(text, terms) {
  normalized <- normalize_text(text)
  matches <- character(0)
  seen <- character(0)
  for (term in terms) {
    term <- trimws(as.character(term))
    if (!nzchar(term)) next
    key <- normalize_text(term)
    if (key %in% seen) next
    if (term_matches(normalized, term)) {
      seen <- c(seen, key)
      matches <- c(matches, term)
    }
  }
  matches
}

#' Does at least one term occur in text?
#'
#' @inheritParams find_terms
#' @return `TRUE`/`FALSE`.
#' @export
any_term_matches <- function(text, terms) {
  normalized <- normalize_text(text)
  terms <- terms[vapply(terms, function(t) nzchar(trimws(as.character(t))), logical(1))]
  if (length(terms) == 0) {
    return(FALSE)
  }
  any(vapply(terms, function(term) term_matches(normalized, as.character(term)), logical(1)))
}

#' Which named term components occur in an article?
#'
#' @param title Article title (may be `NA`/`NULL`).
#' @param body Article body (may be `NA`/`NULL`).
#' @param components A named list mapping component name to a character
#'   vector of terms, e.g. `list(economic = ..., policy = ..., uncertainty = ...)`.
#' @return A named logical vector, one entry per element of `components`.
#' @export
component_matches <- function(title, body, components) {
  full_text <- paste(
    if (is.null(title) || is.na(title)) "" else title,
    if (is.null(body) || is.na(body)) "" else body
  )
  normalized <- normalize_text(full_text)
  vapply(components, function(terms) {
    terms <- terms[vapply(terms, function(t) nzchar(trimws(as.character(t))), logical(1))]
    if (length(terms) == 0) {
      return(FALSE)
    }
    any(vapply(terms, function(term) term_matches(normalized, as.character(term)), logical(1)))
  }, logical(1))
}

#' Classify an article as EPU
#'
#' An article counts as EPU if its title or body contains at least one
#' Economic, one Policy, and one Uncertainty term (Baker, Bloom and Davis,
#' 2016 classification rule).
#'
#' @param title Article title (may be `NA`/`NULL`).
#' @param body Article body (may be `NA`/`NULL`).
#' @param economic_terms,policy_terms,uncertainty_terms Character vectors of
#'   dictionary terms for each component.
#' @return `TRUE`/`FALSE`.
#' @export
is_epu <- function(title, body, economic_terms, policy_terms, uncertainty_terms) {
  flags <- component_matches(title, body, list(
    economic = economic_terms,
    policy = policy_terms,
    uncertainty = uncertainty_terms
  ))
  unname(flags["economic"] && flags["policy"] && flags["uncertainty"])
}
