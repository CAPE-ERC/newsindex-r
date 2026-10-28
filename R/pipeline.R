#' One-call orchestration of the EPU pipeline for non-technical users
#'
#' `run_index()` and `run_index_with_baseline()` wrap [dictionary_load()]
#' and [count_archive()] (which streams the article archive straight into
#' EPU counts, country-month by country-month, never materializing the
#' whole requested scope in memory at once) with
#' [compute_index()]/[fit_baseline()]+[score_windows()] into a single call.
#' Anyone who needs read/dedupe/count as separate, in-memory steps should
#' call [read_articles()], [dedupe_articles()], and [count_articles()]
#' directly instead -- these two exist only to remove the boilerplate
#' around the common case.
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

  .log(verbose, paste0("Streaming articles from ", root, " (one country-month at a time) ..."))
  progress <- if (verbose) function(msg) .log(TRUE, paste0("  ", msg)) else NULL
  counts <- count_archive(root, kw, countries = countries, progress = progress)
  .log(verbose, paste0(
    "Counted ", sum(counts$all_articles), " articles (", sum(counts$epu_articles),
    " EPU) across ", length(unique(counts$country)), " countr(y/ies)."
  ))
  counts
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
  attr(result, "newsindex_config") <- list(
    fn = "run_index", root = root, dictionary_path = dictionary_path,
    start = start, end = end, countries = countries, verbose = verbose
  )
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
  attr(result, "newsindex_config") <- list(
    fn = "run_index_with_baseline", root = root, dictionary_path = dictionary_path,
    windows = windows, countries = countries, verbose = verbose,
    baseline = baseline, baseline_start = baseline_start, baseline_end = baseline_end
  )
  list(result = result, baseline = baseline)
}

#' Run a single country for a single calendar year, no window math required
#'
#' Shortcut for [run_index()] with `start`/`end` set to `"<year>-01"`/
#' `"<year>-12"` and `countries` set to `country`.
#'
#' @param root,dictionary_path As in [run_index()].
#' @param country A single country name.
#' @param year A single calendar year, e.g. `2024`.
#' @param verbose As in [run_index()].
#' @return As [run_index()].
#' @export
quick_index <- function(root, dictionary_path, country, year, verbose = TRUE) {
  run_index(
    root, dictionary_path,
    start = sprintf("%d-01", as.integer(year)), end = sprintf("%d-12", as.integer(year)),
    countries = country, verbose = verbose
  )
}

#' Run every country folder under `root` for a window
#'
#' Explicit alias for [run_index()] with `countries = NULL` (every folder),
#' so the call reads as intent rather than a parameter value.
#'
#' @param root,dictionary_path,start,end,verbose As in [run_index()].
#' @return As [run_index()].
#' @export
index_all <- function(root, dictionary_path, start, end, verbose = TRUE) {
  run_index(root, dictionary_path, start, end, countries = NULL, verbose = verbose)
}

#' Run a named set of countries for a window
#'
#' Alias for [run_index()] naming the `countries` argument explicitly, for
#' discoverability -- [run_index()] already accepts a character vector of
#' country names there.
#'
#' @param root,dictionary_path,start,end,verbose As in [run_index()].
#' @param countries A character vector of country folder names.
#' @return As [run_index()].
#' @export
index_countries <- function(root, dictionary_path, countries, start, end, verbose = TRUE) {
  run_index(root, dictionary_path, start, end, countries = countries, verbose = verbose)
}

#' Run every country in a predefined region for a window
#'
#' `region` is either the name of one group in `groups` (default
#' [AFRICA_REGIONS]), or an explicit character vector of country names
#' (bypassing `groups` entirely).
#'
#' @param root,dictionary_path,start,end,verbose As in [run_index()].
#' @param region A single region name from `names(groups)`, or a character
#'   vector of country names.
#' @param groups A named list of character vectors (default
#'   [AFRICA_REGIONS]).
#' @return As [run_index()].
#' @export
index_region <- function(root, dictionary_path, region, start, end, groups = AFRICA_REGIONS, verbose = TRUE) {
  countries <- if (is.character(region) && length(region) == 1) {
    if (!(region %in% names(groups))) {
      stop(
        "unknown region '", region, "'. Available: ",
        paste(sort(names(groups)), collapse = ", ")
      )
    }
    groups[[region]]
  } else {
    region
  }
  run_index(root, dictionary_path, start, end, countries = countries, verbose = verbose)
}

.month_window <- function(months_back, today = Sys.Date()) {
  if (months_back < 1) {
    stop("months_back must be >= 1")
  }
  end_year <- as.integer(format(today, "%Y"))
  end_month <- as.integer(format(today, "%m"))
  end_total <- end_year * 12L + (end_month - 1L)
  start_total <- end_total - (as.integer(months_back) - 1L)
  start_year <- start_total %/% 12L
  start_month <- start_total %% 12L + 1L
  c(sprintf("%d-%02d", start_year, start_month), sprintf("%d-%02d", end_year, end_month))
}

#' Run a relative window ending at the current calendar month
#'
#' Shortcut for [run_index()] with `start`/`end` computed as the
#' `months_back` months up to and including the current one (`"now"` is the
#' machine's system clock, [Sys.Date()], at call time), instead of explicit
#' `"YYYY-MM"` dates.
#'
#' @param root,dictionary_path,verbose As in [run_index()].
#' @param months_back How many months back from the current month to start
#'   (inclusive of the current month), default `12`.
#' @return As [run_index()].
#' @export
index_since <- function(root, dictionary_path, months_back = 12, verbose = TRUE) {
  window <- .month_window(months_back)
  run_index(root, dictionary_path, window[1], window[2], verbose = verbose)
}

.extract_newsindex_config <- function(x) {
  config <- attr(x, "newsindex_config")
  if (!is.null(config)) {
    return(config)
  }
  if (is.list(x) && !is.data.frame(x) && !is.null(x$fn)) {
    return(x)
  }
  if (is.list(x) && !is.data.frame(x) && !is.null(x$result)) {
    config <- attr(x$result, "newsindex_config")
    if (!is.null(config)) {
      return(config)
    }
  }
  stop(
    "rerun() needs a 'newsindex_config' attribute (attached automatically by run_index(), ",
    "run_index_with_baseline(), quick_index(), index_all(), index_countries(), index_region(), ",
    "and index_since()) or an explicit config list with an 'fn' element. Attributes don't survive ",
    "things like a write.csv()/read.csv() round-trip -- rerun() only works on a value returned ",
    "directly from one of those calls, or a config list you build yourself."
  )
}

#' Re-run the exact call that produced a saved result
#'
#' `previous_result_or_config` is either a result value returned directly
#' from [run_index()], [run_index_with_baseline()], [quick_index()],
#' [index_all()], [index_countries()], [index_region()], or [index_since()]
#' -- all of which attach a `"newsindex_config"` attribute recording the
#' call that produced them -- or an explicit config `list` with the same
#' shape (an `fn` element naming which function to call, plus its
#' arguments).
#'
#' For a `run_index_with_baseline()`-produced result, `rerun()` always
#' reuses the exact `newsindex_baseline` object that scored it (frozen, not
#' refit from `baseline_start`/`baseline_end`), so the re-run reproduces the
#' same scoring even if the underlying archive has since changed.
#'
#' @param previous_result_or_config A result from one of the functions
#'   above, or an explicit config list.
#' @param verbose If given, overrides the stored `verbose` value.
#' @return Whatever the original call returned.
#' @export
rerun <- function(previous_result_or_config, verbose = NULL) {
  config <- .extract_newsindex_config(previous_result_or_config)
  if (is.null(config$fn)) {
    stop("rerun(): config is missing an 'fn' element naming which function to re-run")
  }
  v <- if (is.null(verbose)) {
    if (is.null(config$verbose)) TRUE else config$verbose
  } else {
    verbose
  }

  if (config$fn == "run_index") {
    run_index(config$root, config$dictionary_path, config$start, config$end,
      countries = config$countries, verbose = v
    )
  } else if (config$fn == "run_index_with_baseline") {
    run_index_with_baseline(config$root, config$dictionary_path, config$windows,
      baseline = config$baseline, countries = config$countries, verbose = v
    )
  } else {
    stop("rerun(): unknown fn '", config$fn, "'")
  }
}
