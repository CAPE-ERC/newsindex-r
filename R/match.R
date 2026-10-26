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

.term_body_cache <- new.env(parent = emptyenv())

# The escaped, separator-flexible middle of a term's pattern: its words
# joined by `(?:-|\s)+` so a dash and a space are interchangeable between
# them, same as normalize_text() already folds dash variants and collapses
# whitespace within a single string. No boundary assertions.
.term_body <- function(normalized) {
  cached <- .term_body_cache[[normalized]]
  if (!is.null(cached)) {
    return(cached)
  }
  if (!nzchar(normalized)) {
    body <- "(?!x)x"
  } else {
    pieces <- stringi::stri_split_regex(normalized, "[\\s-]+")[[1]]
    pieces <- pieces[nzchar(pieces)]
    escaped <- vapply(pieces, stringi::stri_replace_all_regex,
      character(1),
      pattern = "([.^$|()\\[\\]{}*+?\\\\-])", replacement = "\\\\$1"
    )
    body <- paste(escaped, collapse = "(?:-|\\s)+")
  }
  .term_body_cache[[normalized]] <- body
  body
}

.term_pattern_cache <- new.env(parent = emptyenv())

.compiled_term_pattern <- function(term) {
  normalized <- normalize_text(term)
  cached <- .term_pattern_cache[[normalized]]
  if (!is.null(cached)) {
    return(cached)
  }
  body <- .term_body(normalized)
  if (!nzchar(normalized)) {
    pattern <- body
  } else {
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

.combined_pattern_cache <- new.env(parent = emptyenv())
.combined_loose_pattern_cache <- new.env(parent = emptyenv())

.distinct_normalized_terms <- function(terms) {
  terms <- terms[vapply(terms, function(t) nzchar(trimws(as.character(t))), logical(1))]
  if (length(terms) == 0) {
    return(character(0))
  }
  terms[!duplicated(vapply(terms, normalize_text, character(1)))]
}

#' One combined regex matching normalized text that contains any of `terms`
#'
#' Equivalent to `any(term_matches(text, term) for term in terms)` but as a
#' single alternation, so a whole vector of articles can be tested against a
#' component in one vectorized call (`stringi::stri_detect_regex`) instead
#' of one call per term per row. Memoized per distinct `terms` vector (as a
#' cache key), since the same dictionary component is reused across every
#' row and batch of a run.
#'
#' This still carries each term's word-boundary assertions, which cost real
#' regex-engine time per character scanned. When most rows won't contain
#' any of `terms` at all (the common case classifying real articles), check
#' [loose_component_pattern()] first and only run this on the rows it
#' flags -- see its docstring.
#'
#' @param terms Character vector of dictionary terms.
#' @return A single regex pattern string (for `stringi::stri_*_regex`).
#' @export
component_pattern <- function(terms) {
  key <- paste(terms, collapse = "\u0001")
  cached <- .combined_pattern_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  distinct <- .distinct_normalized_terms(terms)
  pattern <- if (length(distinct) == 0) {
    "(?!x)x"
  } else {
    paste0("(?:", vapply(distinct, .compiled_term_pattern, character(1)), ")", collapse = "|")
  }
  .combined_pattern_cache[[key]] <- pattern
  pattern
}

#' A cheap, over-inclusive version of [component_pattern()]
#'
#' Same terms, same dash/whitespace flexibility between words, but without
#' the word-boundary lookaround assertions -- so this can only produce
#' false positives relative to [component_pattern()], never false
#' negatives (dropping a check can't make a pattern match less). Matching
#' it is substantially cheaper per character scanned, since there's no
#' lookaround to evaluate at every position.
#'
#' Use it as a prefilter: test a whole vector with this pattern first, then
#' run [component_pattern()] only on the rows it flagged. Most articles
#' don't contain any term from a given component at all, so this avoids
#' ever running the expensive pattern over the great majority of rows.
#'
#' @inheritParams component_pattern
#' @return A single regex pattern string (for `stringi::stri_*_regex`).
#' @export
loose_component_pattern <- function(terms) {
  key <- paste(terms, collapse = "\u0001")
  cached <- .combined_loose_pattern_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  distinct <- .distinct_normalized_terms(terms)
  pattern <- if (length(distinct) == 0) {
    "(?!x)x"
  } else {
    bodies <- vapply(distinct, function(t) .term_body(normalize_text(t)), character(1))
    paste0("(?:", bodies, ")", collapse = "|")
  }
  .combined_loose_pattern_cache[[key]] <- pattern
  pattern
}
