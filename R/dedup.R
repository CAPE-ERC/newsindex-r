#' Article deduplication within a country-month
#'
#' Articles are deduplicated primarily by canonical URL. When the URL is
#' missing or empty, a hash of the normalized title and text stands in for
#' it, so two copies of the same story with no URL still collapse to one
#' row.
#'
#' @name dedup
NULL

.tracking_params <- c(
  "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content",
  "fbclid", "gclid", "mc_cid", "mc_eid"
)

.url_pattern <- "^([a-zA-Z][a-zA-Z0-9+.-]*)://([^/?#]*)([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$"

#' Canonicalize a URL for deduplication
#'
#' Lower-cases the scheme and host, drops the fragment and tracking query
#' parameters, and strips a trailing slash from the path.
#'
#' @param url A character scalar (or `NA`/`NULL`).
#' @return A character scalar: the canonical URL, or `""` if `url` is
#'   missing, blank, or not of the form `scheme://host/...`.
#' @export
canonical_url <- function(url) {
  text <- if (is.null(url) || is.na(url)) "" else trimws(as.character(url))
  if (!nzchar(text)) {
    return("")
  }
  parts <- stringi::stri_match_first_regex(text, .url_pattern)
  if (is.na(parts[1, 1])) {
    return("")
  }
  scheme <- tolower(parts[1, 2])
  netloc <- tolower(parts[1, 3])
  path <- parts[1, 4]
  path <- sub("/+$", "", path)
  query <- parts[1, 5]

  query_str <- ""
  if (!is.na(query) && nzchar(query)) {
    pairs <- strsplit(query, "&", fixed = TRUE)[[1]]
    pairs <- pairs[nzchar(pairs)]
    kv <- lapply(pairs, function(p) {
      idx <- regexpr("=", p, fixed = TRUE)
      if (idx < 0) c(p, "") else c(substr(p, 1, idx - 1), substr(p, idx + 1, nchar(p)))
    })
    keep <- Filter(function(pair) !(tolower(pair[1]) %in% .tracking_params), kv)
    if (length(keep) > 0) {
      keep <- keep[order(vapply(keep, `[`, character(1), 1), vapply(keep, `[`, character(1), 2))]
      query_str <- paste(vapply(keep, function(pair) paste0(pair[1], "=", pair[2]), character(1)),
        collapse = "&"
      )
    }
  }

  out <- paste0(scheme, "://", netloc, path)
  if (nzchar(query_str)) {
    out <- paste0(out, "?", query_str)
  }
  out
}

.hash_blob <- function(blob) {
  paste0("sha256:", digest::digest(blob, algo = "sha256", serialize = FALSE))
}

#' Drop duplicate articles within each group
#'
#' A row is a duplicate of an earlier row in the same group if they share a
#' canonical URL, or, when either URL is missing, if they share a hash of
#' their normalized title and text. The first occurrence in the input is
#' kept.
#'
#' @param articles A data.frame of articles.
#' @param group_cols Character vector of column names defining the group
#'   within which duplicates are detected (default: country and month).
#' @param url_col,title_col,text_col Column names holding the URL, title,
#'   and text.
#' @return `articles`, with later duplicate rows removed and the row index
#'   reset.
#' @export
dedupe_articles <- function(articles, group_cols = c("country", "year_month"),
                             url_col = "url", title_col = "title", text_col = "text") {
  if (nrow(articles) == 0) {
    return(articles)
  }
  missing_cols <- setdiff(group_cols, names(articles))
  if (length(missing_cols) > 0) {
    stop("missing grouping column(s): ", paste(missing_cols, collapse = ", "))
  }

  canonical <- vapply(articles[[url_col]], canonical_url, character(1))

  titles <- as.character(articles[[title_col]])
  titles[is.na(titles)] <- ""
  texts <- as.character(articles[[text_col]])
  texts[is.na(texts)] <- ""
  blob <- paste0(normalize_text(titles), "\u001f", normalize_text(texts))
  fallback <- vapply(blob, .hash_blob, character(1), USE.NAMES = FALSE)

  dedup_key <- ifelse(nzchar(canonical), canonical, fallback)

  group_key <- do.call(paste, c(lapply(group_cols, function(col) articles[[col]]), list(sep = "\u001e")))
  composite <- paste(group_key, dedup_key, sep = "\u001e")

  result <- articles[!duplicated(composite), , drop = FALSE]
  rownames(result) <- NULL
  result
}
