#' One-call orchestration of the EPU pipeline for non-technical users
#'
#' `run_index()` and `run_index_with_baseline()` wrap [dictionary_load()],
#' [read_articles()], [dedupe_articles()], [count_articles()], and
#' [compute_index()]/[fit_baseline()]+[score_windows()] into a single call.
#' Anyone who needs one of those steps to behave differently should call
#' the underlying functions directly instead -- these two exist only to
#' remove the boilerplate around the common case.
#'
#' @name pipeline
NULL

.log <- function(verbose, message) {
  if (verbose) message(message)
}

.load_dictionary <- function(dictionary_path, verbose) {
  if (!file.exists(dictionary_path)) {
    stop("dictionary file not found: ", dictionary_path)
  }
  .log(verbose, paste0("Loading dictionary from ", dictionary_path, " ..."))
  dictionary_load(dictionary_path)
}

.read_and_count <- function(root, dictionary_path, countries, verbose) {
  kw <- .load_dictionary(dictionary_path, verbose)

  .log(verbose, paste0("Reading articles from ", root, " ..."))
  articles <- read_articles(root, countries = countries)
  .log(verbose, paste0(
    "Read ", nrow(articles), " articles across ", length(unique(articles$country)),
    " country folder(s)."
  ))

  .log(verbose, "Deduplicating ...")
  deduped <- dedupe_articles(articles)
  .log(verbose, paste0(nrow(deduped), " articles remain after deduplication."))

  .log(verbose, "Counting EPU articles per newspaper-month ...")
  count_articles(deduped, kw)
}

#' Read an article archive and compute a single-window EPU index in one call
#'
#' `root` is the article archive directory (see [read_articles()]);
#' `dictionary_path` the EPTU-schema keyword dictionary to classify
#' against. `start`/`end` are the inclusive `"YYYY-MM"` window: `sigma`/`mu`
#' are fit from it and used to score that same window -- this is the
#' no-baseline variant (see [compute_index()]).
#'
#' @param root Article archive directory.
#' @param dictionary_path Path to an EPTU-schema keyword dictionary. Always
#'   an explicit argument -- this package has no bundled default.
#' @param start,end Inclusive `"YYYY-MM"` window.
#' @param countries Forwarded to [read_articles()]: a character vector, or
#'   `"all"`/`NULL` (default) for every folder.
#' @param verbose If `TRUE` (default), prints progress.
#' @return A data.frame as returned by [compute_index()].
#' @export
run_index <- function(root, dictionary_path, start, end, countries = NULL, verbose = TRUE) {
  counts <- .read_and_count(root, dictionary_path, countries, verbose)
  .log(verbose, paste0("Computing index for ", start, "..", end, " ..."))
  result <- compute_index(counts, start, end)
  .log(verbose, paste0("Done: ", nrow(result), " country-month rows."))
  result
}

#' Read an article archive and score it against a frozen baseline in one call
#'
#' `root` and `dictionary_path` are as in [run_index()]. Either pass an
#' already-fitted `baseline` (e.g. from [baseline_load()]) to reuse it, or
#' pass `baseline_start`/`baseline_end` to fit a new one from this archive
#' -- exactly one of the two must be given. `windows` is a list of
#' inclusive `c(start, end)` pairs scored against that baseline (see
#' [score_windows()]); this is the with-baseline variant.
#'
#' @param root Article archive directory.
#' @param dictionary_path Path to an EPTU-schema keyword dictionary. Always
#'   an explicit argument -- this package has no bundled default.
#' @param windows A list of two-element character vectors, each
#'   `c(start, end)` (inclusive `"YYYY-MM"` strings).
#' @param baseline Optional already-fitted `newsindex_baseline` to reuse.
#' @param baseline_start,baseline_end Inclusive `"YYYY-MM"` window to fit a
#'   new baseline from, when `baseline` is not given.
#' @param countries Forwarded to [read_articles()]: a character vector, or
#'   `"all"`/`NULL` (default) for every folder.
#' @param verbose If `TRUE` (default), prints progress.
#' @return A list with `result` (as returned by [score_windows()]) and
#'   `baseline` (the baseline used, so a freshly fit one can be saved with
#'   [baseline_save()]).
#' @export
run_index_with_baseline <- function(root, dictionary_path, windows, baseline = NULL,
                                     baseline_start = NULL, baseline_end = NULL,
                                     countries = NULL, verbose = TRUE) {
  if (is.null(baseline) && (is.null(baseline_start) || is.null(baseline_end))) {
    stop("pass either baseline, or both baseline_start and baseline_end, to fit one")
  }
  if (!is.null(baseline) && (!is.null(baseline_start) || !is.null(baseline_end))) {
    stop("pass baseline on its own, not alongside baseline_start/baseline_end")
  }

  counts <- .read_and_count(root, dictionary_path, countries, verbose)

  if (is.null(baseline)) {
    .log(verbose, paste0("Fitting baseline ", baseline_start, "..", baseline_end, " ..."))
    baseline <- fit_baseline(counts, baseline_start, baseline_end)
  } else {
    .log(verbose, paste0("Using provided baseline (", baseline$start, "..", baseline$end, ")."))
  }

  .log(verbose, paste0("Scoring ", length(windows), " window(s) against the baseline ..."))
  result <- score_windows(counts, baseline, windows)
  .log(verbose, paste0("Done: ", nrow(result), " country-month rows."))
  list(result = result, baseline = baseline)
}
