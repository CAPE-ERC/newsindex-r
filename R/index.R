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

#' Count EPU and total articles per country-site-month
#'
#' Every article's country must be present in `dictionary`; an error is
#' raised naming any country that is not, since a missing country silently
#' produces an undercount rather than a visible failure.
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

  flags <- vapply(seq_len(nrow(articles)), function(i) {
    entry <- dictionary[[articles[[country_col]][i]]]
    is_epu(articles[[title_col]][i], articles[[text_col]][i], entry$economic, entry$policy, entry$uncertainty)
  }, logical(1))

  working <- data.frame(
    country = articles[[country_col]],
    site = articles[[site_col]],
    year_month = articles[[month_col]],
    epu_articles = as.integer(flags),
    all_articles = 1L,
    stringsAsFactors = FALSE
  )
  grouped <- stats::aggregate(
    cbind(epu_articles, all_articles) ~ country + site + year_month,
    data = working, FUN = sum
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
