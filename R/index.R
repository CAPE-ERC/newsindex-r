#' Count EPU articles per newspaper-month, fit the frozen baseline, and
#' score new months
#'
#' Implements the Baker, Bloom and Davis (2016) scaling method documented in
#' the CAPE Africa EPU methodology: for newspaper *i* in month *t*, `S_it`
#' is the EPU article share, `N_it = S_it / sigma_i` standardises it against
#' the newspaper's baseline standard deviation, `M_t` is the mean of `N_it`
#' across newspapers, and `EPU_t = 100 * M_t / mu` rescales to a baseline
#' mean of 100. `sigma_i`, `mu`, and the retained newspapers are frozen once
#' at [fit_baseline()] time; [score_index()] never recomputes them.
#'
#' @name index
NULL

.population_sd <- function(x) sqrt(mean((x - mean(x))^2))

# Logical mask over `text` (already normalized): does each article contain
# any of `terms`? Two-stage: loose_component_pattern() first (cheap,
# over-inclusive) to find candidates, then component_pattern() (precise,
# word-boundary-checked) only on those -- see their docstrings. Equivalent
# to `stringi::stri_detect_regex(text, component_pattern(terms))` directly,
# just far cheaper when most rows don't contain any of `terms` at all.
.component_survivors_local <- function(text, terms) {
  candidate_mask <- stringi::stri_detect_regex(text, loose_component_pattern(terms))
  if (!any(candidate_mask)) {
    return(candidate_mask)
  }
  result <- candidate_mask
  result[candidate_mask] <- stringi::stri_detect_regex(text[candidate_mask], component_pattern(terms))
  result
}

#' Count EPU and total articles per country-site-month
#'
#' Every article's country must be present in `dictionary`; an error is
#' raised naming any country that is not, since a missing country silently
#' produces an undercount rather than a visible failure.
#'
#' Classification is vectorized and short-circuits: per country, each
#' component (Economic, Policy, Uncertainty) is checked only against the
#' rows that survived the previous component, since EPU requires all
#' three, using the cheap-then-precise two-stage pattern (see
#' [loose_component_pattern()]). This avoids both the per-row/per-term loop
#' and the full boundary-checked regex cost on articles that plainly don't
#' qualify, which is most of them.
#'
#' @param articles A data.frame of articles.
#' @param dictionary A `newsindex_dictionary` from [dictionary_load()].
#' @param country_col,site_col,month_col,title_col,text_col Column names in
#'   `articles`.
#' @return A data.frame with columns `country`, `site`, `year_month`,
#'   `epu_articles`, `all_articles`.
#' @export
count_articles <- function(articles, dictionary, country_col = "country", site_col = "site",
                            month_col = "year_month", title_col = "title", text_col = "text") {
  missing <- sort(setdiff(unique(articles[[country_col]]), names(dictionary)))
  if (length(missing) > 0) {
    stop("countries missing from dictionary: ", paste(missing, collapse = ", "))
  }

  title <- as.character(articles[[title_col]])
  title[is.na(title)] <- ""
  text <- as.character(articles[[text_col]])
  text[is.na(text)] <- ""
  normalized <- vapply(paste(title, text), normalize_text, character(1), USE.NAMES = FALSE)

  countries_col <- as.character(articles[[country_col]])
  is_epu_flags <- logical(nrow(articles))
  for (country in unique(countries_col)) {
    rows <- which(countries_col == country)
    entry <- dictionary[[country]]
    survivors <- rep(TRUE, length(rows))
    for (terms in list(entry$economic, entry$policy, entry$uncertainty)) {
      if (!any(survivors)) break
      survivors[survivors] <- .component_survivors_local(normalized[rows[survivors]], terms)
    }
    is_epu_flags[rows[survivors]] <- TRUE
  }

  # Plain character, not factor: read_articles() returns country/site/
  # year_month as factors to save memory on the large per-article table,
  # but aggregate()'s formula interface groups over *all* factor levels
  # (including ones absent here), which would fabricate empty combinations.
  working <- data.frame(
    country = as.character(articles[[country_col]]),
    site = as.character(articles[[site_col]]),
    year_month = as.character(articles[[month_col]]),
    epu_articles = as.integer(is_epu_flags),
    all_articles = 1L,
    stringsAsFactors = FALSE
  )
  grouped <- stats::aggregate(
    cbind(epu_articles, all_articles) ~ country + site + year_month,
    data = working, FUN = sum
  )
  grouped[order(grouped$country, grouped$site, grouped$year_month), ]
}

#' Count EPU and total articles per country-site-month directly from an
#' article archive
#'
#' Goes straight from the archive root to the final per-newspaper-month
#' counts, without ever materializing the whole requested scope in memory
#' at once.
#'
#' Equivalent to `count_articles(dedupe_articles(read_articles(root,
#' countries = countries)), dictionary)` with the default `dedupe = TRUE`,
#' but processes one `(country, year-month)` batch at a time -- see
#' [country_month_batches()] -- discarding each batch's raw article text as
#' soon as it's been counted. Peak memory stays close to the size of one
#' country-month's articles, regardless of how many countries, newspapers,
#' or years `root` spans. This is what [run_index()] and
#' [run_index_with_baseline()] use internally.
#'
#' Deduplication happens within each (country, year-month folder) batch,
#' matching the archive's own month-folder organization -- equivalent to
#' deduplicating the whole requested scope at once, unless an article's
#' actual date disagrees with the month folder it's stored in, in which
#' case only within-batch duplicates are caught. Pass `dedupe = FALSE` to
#' skip this and deduplicate the full table yourself instead, if that
#' distinction matters for your archive.
#'
#' @param root Path to the top-level archive directory.
#' @param dictionary A `newsindex_dictionary` from [dictionary_load()].
#' @param countries As in [read_articles()].
#' @param encoding File encoding (default `"UTF-8"`).
#' @param dedupe If `TRUE` (default), deduplicate each batch before
#'   counting it.
#' @param country_col,site_col,month_col,title_col,text_col Column names,
#'   passed through to [count_articles()].
#' @param progress If given, a function called with one short status
#'   string after each batch (e.g. to print it) -- useful for a long run
#'   against a large archive, so it's visible as working rather than
#'   looking stalled.
#' @return A data.frame with columns `country`, `site`, `year_month`,
#'   `epu_articles`, `all_articles`.
#' @export
count_archive <- function(root, dictionary, countries = NULL, encoding = "UTF-8", dedupe = TRUE,
                           country_col = "country", site_col = "site", month_col = "year_month",
                           title_col = "title", text_col = "text", progress = NULL) {
  process_batch <- function(batch) {
    country <- if (nrow(batch) > 0) as.character(batch[[country_col]][1]) else "?"
    months <- if (nrow(batch) > 0) sort(unique(as.character(batch[[month_col]]))) else character(0)
    n_read <- nrow(batch)

    if (dedupe) {
      batch <- dedupe_articles(batch, url_col = "url", title_col = title_col, text_col = text_col)
    }
    n_deduped <- nrow(batch)

    batch_counts <- count_articles(
      batch, dictionary,
      country_col = country_col, site_col = site_col, month_col = month_col,
      title_col = title_col, text_col = text_col
    )

    if (!is.null(progress)) {
      epu_total <- sum(batch_counts$epu_articles)
      month_label <- if (length(months) > 0) paste(months, collapse = ",") else "?"
      progress(sprintf(
        "%s %s: %s articles (%s after dedup), %s EPU",
        country, month_label, format(n_read, big.mark = ","),
        format(n_deduped, big.mark = ","), format(epu_total, big.mark = ",")
      ))
    }
    batch_counts
  }

  parts <- country_month_batches(root, process_batch, countries = countries, encoding = encoding)
  combined <- do.call(rbind, parts)
  grouped <- stats::aggregate(
    cbind(epu_articles, all_articles) ~ country + site + year_month,
    data = combined, FUN = sum
  )
  grouped[order(grouped$country, grouped$site, grouped$year_month), ]
}

#' @export
print.newsindex_baseline <- function(x, ...) {
  cat(sprintf(
    "<newsindex_baseline> %s to %s, %d countries\n",
    x$start, x$end, length(x$mu)
  ))
  invisible(x)
}

#' Retained newspapers for one country's baseline
#'
#' @param baseline A `newsindex_baseline` from [fit_baseline()] or
#'   [baseline_load()].
#' @param country A country name.
#' @return A sorted character vector of newspaper names.
#' @export
baseline_retained_sites <- function(baseline, country) {
  sites <- baseline$sigma[[country]]
  if (is.null(sites)) character(0) else sort(names(sites))
}

#' Fit newspaper standard deviations and country means over `[start, end]`
#'
#' A newspaper is retained for a country's baseline if it has at least two
#' monthly observations in the window and a nonzero population standard
#' deviation of its EPU article share; a newspaper with constant or
#' single-month coverage cannot be standardised and is dropped.
#'
#' @param counts A data.frame from [count_articles()].
#' @param start,end `"YYYY-MM"` strings bounding the baseline window
#'   (inclusive).
#' @param dictionary_fingerprint Optional fingerprint (from
#'   [dictionary_fingerprint()]) recorded on the baseline.
#' @return A `newsindex_baseline`.
#' @export
fit_baseline <- function(counts, start, end, dictionary_fingerprint = "") {
  window <- counts[counts$year_month >= start & counts$year_month <= end & counts$all_articles > 0, ]
  window$S_it <- window$epu_articles / window$all_articles

  sigma <- list()
  pairs <- unique(window[, c("country", "site")])
  for (i in seq_len(nrow(pairs))) {
    country <- pairs$country[i]
    site <- pairs$site[i]
    values <- window$S_it[window$country == country & window$site == site]
    if (length(values) < 2) next
    sd <- .population_sd(values)
    if (sd > 0) {
      sigma[[country]][site] <- sd
    }
  }

  mu <- numeric(0)
  for (country in names(sigma)) {
    site_sigma <- sigma[[country]]
    country_rows <- window[window$country == country & window$site %in% names(site_sigma), ]
    country_rows$N_it <- country_rows$S_it / site_sigma[country_rows$site]
    m_t <- stats::aggregate(N_it ~ year_month, data = country_rows, FUN = mean)
    if (nrow(m_t) > 0) {
      mu[country] <- mean(m_t$N_it)
    }
  }
  sigma <- sigma[names(sigma) %in% names(mu)]

  structure(
    list(
      start = start, end = end, mu = mu, sigma = sigma,
      dictionary_fingerprint = dictionary_fingerprint,
      library_version = .newsindex_version
    ),
    class = "newsindex_baseline"
  )
}

#' Score every country-month in `counts` against a frozen baseline
#'
#' @param counts A data.frame from [count_articles()].
#' @param baseline A `newsindex_baseline` from [fit_baseline()] or
#'   [baseline_load()].
#' @return A data.frame with columns `country`, `year_month`, `EPU`, `M_t`,
#'   `mu`, `n_sites`.
#' @export
score_index <- function(counts, baseline) {
  working <- counts[counts$all_articles > 0, ]
  working$S_it <- working$epu_articles / working$all_articles

  rows <- list()
  for (country in names(baseline$mu)) {
    site_sigma <- baseline$sigma[[country]]
    country_rows <- working[working$country == country & working$site %in% names(site_sigma), ]
    if (nrow(country_rows) == 0) next
    country_rows$N_it <- country_rows$S_it / site_sigma[country_rows$site]
    for (ym in sort(unique(country_rows$year_month))) {
      month_rows <- country_rows[country_rows$year_month == ym, ]
      m_t <- mean(month_rows$N_it)
      rows[[length(rows) + 1]] <- data.frame(
        country = country, year_month = ym,
        EPU = 100 * m_t / baseline$mu[[country]],
        M_t = m_t, mu = baseline$mu[[country]],
        n_sites = length(unique(month_rows$site)),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      country = character(), year_month = character(), EPU = numeric(),
      M_t = numeric(), mu = numeric(), n_sites = integer()
    ))
  }
  result <- do.call(rbind, rows)
  result[order(result$country, result$year_month), ]
}

#' Compute an in-sample index over a single window (the no-baseline variant)
#'
#' Fits `sigma`/`mu` from `[start, end]` and scores that same window against
#' them; nothing is frozen or reused for a later period. Equivalent to
#' `score_index(window, fit_baseline(window, start, end))` where `window` is
#' `counts` restricted to `[start, end]`, but callers never see or manage a
#' `newsindex_baseline`.
#'
#' @param counts A data.frame from [count_articles()].
#' @param start,end `"YYYY-MM"` strings bounding the single window
#'   (inclusive).
#' @return A data.frame with columns `country`, `year_month`, `EPU`, `M_t`,
#'   `mu`, `n_sites`.
#' @export
compute_index <- function(counts, start, end) {
  window <- counts[counts$year_month >= start & counts$year_month <= end, ]
  baseline <- fit_baseline(window, start, end)
  score_index(window, baseline)
}

#' Score one or more later windows against a frozen baseline (the
#' with-baseline variant)
#'
#' `windows` is typically just one period that follows the baseline window,
#' but any number may be given; they may also overlap or leave gaps between
#' them. The result adds a `window` column (`"start:end"`) identifying which
#' pair produced each row.
#'
#' @param counts A data.frame from [count_articles()].
#' @param baseline A `newsindex_baseline` from [fit_baseline()] or
#'   [baseline_load()].
#' @param windows A list of two-element character vectors, each
#'   `c(start, end)` (inclusive `"YYYY-MM"` strings).
#' @return A data.frame with columns `window`, `country`, `year_month`,
#'   `EPU`, `M_t`, `mu`, `n_sites`.
#' @export
score_windows <- function(counts, baseline, windows) {
  columns <- c("window", "country", "year_month", "EPU", "M_t", "mu", "n_sites")
  if (length(windows) == 0) {
    empty <- as.data.frame(stats::setNames(
      list(character(), character(), character(), numeric(), numeric(), numeric(), integer()),
      columns
    ))
    return(empty)
  }

  parts <- lapply(windows, function(w) {
    start <- w[1]
    end <- w[2]
    window_counts <- counts[counts$year_month >= start & counts$year_month <= end, ]
    scored <- score_index(window_counts, baseline)
    scored$window <- paste0(start, ":", end)
    scored[, columns]
  })
  result <- do.call(rbind, parts)
  result[order(result$window, result$country, result$year_month), ]
}

#' Save a baseline to JSON
#'
#' @param baseline A `newsindex_baseline`.
#' @param path Destination file path.
#' @return `invisible(path)`.
#' @export
baseline_save <- function(baseline, path) {
  countries <- sort(names(baseline$mu))
  payload <- list(
    start = baseline$start,
    end = baseline$end,
    dictionary_fingerprint = baseline$dictionary_fingerprint,
    library_version = baseline$library_version,
    countries = stats::setNames(
      lapply(countries, function(country) {
        list(mu = baseline$mu[[country]], sites = as.list(baseline$sigma[[country]]))
      }),
      countries
    )
  )
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), path)
  invisible(path)
}

#' Load a baseline from JSON
#'
#' @param path Path to a file written by [baseline_save()].
#' @return A `newsindex_baseline`.
#' @export
baseline_load <- function(path) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  countries <- payload$countries
  mu <- stats::setNames(
    vapply(countries, function(row) as.numeric(row$mu), numeric(1)),
    names(countries)
  )
  sigma <- stats::setNames(
    lapply(countries, function(row) {
      sites <- row$sites
      stats::setNames(vapply(sites, as.numeric, numeric(1)), names(sites))
    }),
    names(countries)
  )
  structure(
    list(
      start = payload$start, end = payload$end, mu = mu, sigma = sigma,
      dictionary_fingerprint = if (is.null(payload$dictionary_fingerprint)) "" else payload$dictionary_fingerprint,
      library_version = if (is.null(payload$library_version)) "" else payload$library_version
    ),
    class = "newsindex_baseline"
  )
}
