.required_article_cols <- c("site", "date", "title", "url", "text")

.resolve_countries <- function(root, countries) {
  available <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  if (is.null(countries) || identical(countries, "all")) {
    return(available)
  }
  by_lower <- stats::setNames(available, tolower(available))
  matched <- by_lower[tolower(countries)]
  missing <- countries[is.na(matched)]
  if (length(missing) > 0) {
    stop(
      "country folder(s) not found under ", root, ": ",
      paste(missing, collapse = ", "), ". Available: ", paste(available, collapse = ", ")
    )
  }
  unname(matched)
}

.read_one_article_file <- function(file, country, encoding) {
  frame <- utils::read.csv(file, encoding = encoding, stringsAsFactors = FALSE)
  missing <- setdiff(.required_article_cols, names(frame))
  if (length(missing) > 0) {
    stop(file, ": missing column(s) ", paste(missing, collapse = ", "))
  }
  frame$country <- country
  frame
}

.finalize_articles <- function(frame) {
  frame$title <- .html_unescape(frame$title)
  frame$text <- .html_unescape(frame$text)
  frame$year_month <- substr(as.character(frame$date), 1, 7)
  frame <- frame[, c("country", "site", "year_month", "date", "title", "url", "text")]
  for (col in c("country", "site", "year_month")) {
    frame[[col]] <- factor(frame[[col]])
  }
  frame
}

#' Read scraped article archives into a single table
#'
#' Expects the on-disk layout produced by CAPE's scraper:
#' `<root>/<country>/<newspaper>/<year-month>/articles_<newspaper>_<year-month>.csv(.gz)`,
#' each file holding the columns `site`, `date`, `title`, `url`, `text`.
#' This is one of two functions in the package that take a path rather than
#' a table, matching the documented example usage.
#'
#' @param root Path to the top-level archive directory.
#' @param countries Restricts which top-level country folders are read:
#'   `NULL` or `"all"` (the default) reads every folder under `root`; a
#'   character vector reads only the named folders (matched
#'   case-insensitively against the folder names actually present, since
#'   archive folder names don't always match a dictionary's or
#'   [countries()]'s casing). An unmatched name raises an error listing the
#'   folders that were found, to catch typos early.
#' @param encoding File encoding (default `"UTF-8"`).
#' @return A data.frame with columns `country`, `site`, `year_month`,
#'   `date`, `title`, `url`, `text`. Titles and text are HTML-unescaped
#'   (the scraper stores raw HTML entities such as `&#8217;`) before they
#'   reach [normalize_text()]. `country`/`site`/`year_month` are factors
#'   (each distinct value stored once, not once per row) to keep the
#'   combined table's memory footprint down.
#'
#'   This reads the whole requested scope into memory at once. For a large
#'   archive, prefer [count_archive()], which never holds more than one
#'   country-month of articles at a time.
#' @export
read_articles <- function(root, countries = NULL, encoding = "UTF-8") {
  selected <- .resolve_countries(root, countries)

  files <- unlist(lapply(selected, function(country) {
    list.files(file.path(root, country), pattern = "\\.csv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
  }))
  if (length(files) == 0) {
    stop("no article files found under ", root)
  }
  files <- sort(files)

  root_norm <- normalizePath(root, winslash = "/", mustWork = TRUE)
  frames <- lapply(files, function(file) {
    file_norm <- normalizePath(file, winslash = "/", mustWork = TRUE)
    rel <- sub(paste0("^", root_norm, "/"), "", file_norm)
    country <- strsplit(rel, "/", fixed = TRUE)[[1]][1]
    .read_one_article_file(file, country, encoding)
  })

  .finalize_articles(do.call(rbind, frames))
}

#' Apply a function to one (country, year-month) batch of articles at a time
#'
#' Instead of materializing the whole requested scope in memory at once
#' (like [read_articles()] does), this walks the archive's own layout --
#' every newspaper's files under one `<country>/*/<year-month>/` folder are
#' read and passed to `FUN` together, then dropped before moving to the
#' next (country, year-month) pair -- so peak memory stays close to the
#' size of one country-month's articles, regardless of how many countries,
#' newspapers, or years the overall scope spans. This requires the exact
#' documented two-level `<newspaper>/<year-month>/` nesting under each
#' country folder, since the year-month *folder* name is what batches are
#' grouped by; each row's own `year_month` column is still derived from its
#' actual `date` value, exactly as in [read_articles()] -- the folder name
#' only decides batch boundaries.
#'
#' @param root Path to the top-level archive directory.
#' @param FUN A function taking one batch (a data.frame, same shape as
#'   [read_articles()]'s return value) and returning anything.
#' @param countries As in [read_articles()].
#' @param encoding File encoding (default `"UTF-8"`).
#' @return A list of `FUN(batch)` results, one per (country, year-month)
#'   batch, in the order processed (countries in `countries`/directory
#'   order, year-months sorted within each country).
#' @export
country_month_batches <- function(root, FUN, countries = NULL, encoding = "UTF-8") {
  selected <- .resolve_countries(root, countries)

  results <- list()
  found_any <- FALSE
  for (country in selected) {
    country_root <- file.path(root, country)
    files <- list.files(country_root, pattern = "\\.csv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) next
    year_months <- basename(dirname(files))

    for (year_month in sort(unique(year_months))) {
      month_files <- files[year_months == year_month]
      frames <- lapply(month_files, function(file) .read_one_article_file(file, country, encoding))
      batch <- .finalize_articles(do.call(rbind, frames))
      found_any <- TRUE
      results[[length(results) + 1]] <- FUN(batch)
    }
  }

  if (!found_any) {
    stop("no article files found under ", root)
  }
  results
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
