#' Load and validate country keyword dictionaries for EPU classification
#'
#' The on-disk format is the EPTU schema used across CAPE's EPU, ETU, and
#' TPU indices: one JSON document with a `countries` map, each entry
#' carrying a language list and Economic / Policy / Trade / Uncertainty
#' term lists. A term belongs to exactly one component; [dictionary_validate()]
#' checks that no literal term, once normalized, is shared across the
#' Economic, Policy, and Uncertainty components used by the EPU index.
#'
#' @name dictionary
NULL

.epu_components <- c("economic", "policy", "uncertainty")

#' Load a country keyword dictionary
#'
#' @param path Path to an EPTU-schema JSON file.
#' @return A `newsindex_dictionary`: a named list of per-country term lists
#'   (`$languages`, `$economic`, `$policy`, `$uncertainty`, `$trade`), with
#'   `attr(, "schema")`, `attr(, "source")`, and `attr(, "meta")`.
#' @export
dictionary_load <- function(path) {
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)
  rows <- if (!is.null(raw[["countries"]])) raw[["countries"]] else raw

  as_chr <- function(x) if (is.null(x)) character(0) else vapply(x, as.character, character(1))

  countries <- lapply(rows, function(payload) {
    list(
      languages = as_chr(payload[["languages"]]),
      economic = as_chr(payload[["ECONOMY_TERMS"]]),
      policy = as_chr(payload[["POLICY_TERMS"]]),
      uncertainty = as_chr(payload[["UNCERTAINTY_TERMS"]]),
      trade = as_chr(payload[["TRADE_TERMS"]])
    )
  })
  names(countries) <- names(rows)

  meta <- raw[["_meta"]]
  structure(countries,
    class = "newsindex_dictionary",
    schema = if (!is.null(meta[["schema"]])) meta[["schema"]] else "",
    source = path,
    meta = meta
  )
}

#' @export
print.newsindex_dictionary <- function(x, ...) {
  cat(sprintf(
    "<newsindex_dictionary> %d countries, schema %s\n",
    length(x), attr(x, "schema")
  ))
  invisible(x)
}

#' Audit a dictionary for empty components and cross-component overlap
#'
#' Returns one row per issue (an empty list if the dictionary is clean).
#' A term that normalizes to the same text in two of
#' `{economic, policy, uncertainty}` violates the rule that one term may
#' satisfy only one index component.
#'
#' @param dictionary A `newsindex_dictionary` from [dictionary_load()].
#' @return A list of issue rows, each a named list with `country`, `issue`,
#'   and supporting detail (`component`, or `components`/`terms`).
#' @export
dictionary_validate <- function(dictionary) {
  issues <- list()
  for (country in names(dictionary)) {
    entry <- dictionary[[country]]
    normalized <- lapply(.epu_components, function(name) {
      terms <- entry[[name]]
      terms <- terms[nzchar(trimws(terms))]
      if (length(terms) == 0) {
        issues[[length(issues) + 1]] <<- list(country = country, issue = "empty_component", component = name)
      }
      unique(vapply(terms, normalize_text, character(1)))
    })
    names(normalized) <- .epu_components

    for (i in seq_along(.epu_components)) {
      for (j in seq_len(i - 1)) {
        left <- .epu_components[i]
        right <- .epu_components[j]
        overlap <- sort(intersect(normalized[[left]], normalized[[right]]))
        if (length(overlap) > 0) {
          issues[[length(issues) + 1]] <- list(
            country = country,
            issue = "cross_component_overlap",
            components = paste(sort(c(left, right)), collapse = "/"),
            terms = overlap
          )
        }
      }
    }
  }
  issues
}

#' A stable hash identifying the exact dictionary contents
#'
#' Two dictionaries with the same country, language, and term content hash
#' identically regardless of key order; any change to a term list, a
#' language list, or the set of countries changes the fingerprint. Baselines
#' record this value so a later, differently worded dictionary cannot be
#' silently reused to score frozen history.
#'
#' @param dictionary A `newsindex_dictionary` from [dictionary_load()].
#' @return A character scalar: the hex-encoded SHA-256 hash.
#' @export
dictionary_fingerprint <- function(dictionary) {
  countries <- names(dictionary)
  canonical <- lapply(sort(countries), function(country) {
    entry <- dictionary[[country]]
    list(
      languages = sort(entry$languages),
      economic = sort(vapply(entry$economic, normalize_text, character(1))),
      policy = sort(vapply(entry$policy, normalize_text, character(1))),
      uncertainty = sort(vapply(entry$uncertainty, normalize_text, character(1))),
      trade = sort(vapply(entry$trade, normalize_text, character(1)))
    )
  })
  names(canonical) <- sort(countries)
  blob <- jsonlite::toJSON(canonical, auto_unbox = TRUE, null = "null")
  digest::digest(as.character(blob), algo = "sha256", serialize = FALSE)
}
