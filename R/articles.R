#' Read scraped article archives into a single table
#'
#' Expects the on-disk layout produced by CAPE's scraper:
#' `<root>/<country>/<newspaper>/<year-month>/articles_<newspaper>_<year-month>.csv(.gz)`,
#' each file holding the columns `site`, `date`, `title`, `url`, `text`.
#' This is the one function in the package that takes a path rather than a
#' table, matching the documented example usage.
#'
#' @param root Path to the top-level archive directory.
#' @param encoding File encoding (default `"UTF-8"`).
#' @return A data.frame with columns `country`, `site`, `year_month`,
#'   `date`, `title`, `url`, `text`. Titles and text are HTML-unescaped
#'   (the scraper stores raw HTML entities such as `&#8217;`) before they
#'   reach [normalize_text()].
#' @export
read_articles <- function(root, encoding = "UTF-8") {
  files <- list.files(root, pattern = "\\.csv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop("no article files found under ", root)
  }
  files <- sort(files)

  required_cols <- c("site", "date", "title", "url", "text")
  root_norm <- normalizePath(root, winslash = "/", mustWork = TRUE)

  frames <- lapply(files, function(file) {
    file_norm <- normalizePath(file, winslash = "/", mustWork = TRUE)
    rel <- sub(paste0("^", root_norm, "/"), "", file_norm)
    country <- strsplit(rel, "/", fixed = TRUE)[[1]][1]

    frame <- utils::read.csv(file, encoding = encoding, stringsAsFactors = FALSE)
    missing <- setdiff(required_cols, names(frame))
    if (length(missing) > 0) {
      stop(file, ": missing column(s) ", paste(missing, collapse = ", "))
    }
    frame$country <- country
    frame
  })

  combined <- do.call(rbind, frames)
  combined$title <- .html_unescape(combined$title)
  combined$text <- .html_unescape(combined$text)
  combined$year_month <- substr(as.character(combined$date), 1, 7)
  combined[, c("country", "site", "year_month", "date", "title", "url", "text")]
}

.named_entities <- c(
  "amp" = "&", "lt" = "<", "gt" = ">", "quot" = "\"", "apos" = "'",
  "nbsp" = "\u00a0", "hellip" = "\u2026", "mdash" = "\u2014", "ndash" = "\u2013",
  "rsquo" = "\u2019", "lsquo" = "\u2018", "rdquo" = "\u201d", "ldquo" = "\u201c",
  "copy" = "\u00a9", "reg" = "\u00ae", "trade" = "\u2122"
)

#' @keywords internal
.html_unescape <- function(x) {
  x <- as.character(x)

  m <- gregexpr("&#([0-9]+);", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(matches) {
    if (length(matches) == 0) {
      return(matches)
    }
    codes <- as.integer(sub("&#([0-9]+);", "\\1", matches))
    vapply(codes, function(cp) tryCatch(intToUtf8(cp), error = function(e) ""), character(1))
  })

  m <- gregexpr("&#[xX]([0-9A-Fa-f]+);", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(matches) {
    if (length(matches) == 0) {
      return(matches)
    }
    codes <- strtoi(sub("&#[xX]([0-9A-Fa-f]+);", "\\1", matches), base = 16L)
    vapply(codes, function(cp) tryCatch(intToUtf8(cp), error = function(e) ""), character(1))
  })

  for (name in setdiff(names(.named_entities), "amp")) {
    x <- stringi::stri_replace_all_fixed(x, paste0("&", name, ";"), .named_entities[[name]])
  }
  stringi::stri_replace_all_fixed(x, "&amp;", "&")
}
