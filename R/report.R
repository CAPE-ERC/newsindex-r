#' Turn a raw EPU result table into something readable without touching it
#' as a data.frame directly
#'
#' These operate on a result data.frame with `country`, `year_month`, `EPU`
#' columns, as returned by [score_index()]/[compute_index()]/
#' [score_windows()]/[run_index()]/[run_index_with_baseline()]. Plain
#' result tables are not given a custom print method (too invasive for a
#' data.frame) -- [summarize()], [describe()], and [print_period()] are the
#' "nicer view" instead.
#'
#' @name report
NULL

#' Plain-English summary of an EPU result table
#'
#' One line per country (sorted order): the first and last EPU value (with
#' their `year_month`), the percent change from first to last, and the
#' single month-over-month move with the largest absolute change. A
#' country with fewer than two months of data reports only its single
#' value.
#'
#' @param result A data.frame with `country`, `year_month`, `EPU` columns.
#' @return A single character string (not printed -- assign it, or wrap it
#'   in `cat()`/`message()` to display it).
#' @export
summarize <- function(result) {
  countries <- sort(unique(result$country))
  blocks <- vapply(countries, function(country) {
    rows <- result[result$country == country, ]
    rows <- rows[order(rows$year_month), ]
    if (nrow(rows) == 1) {
      return(sprintf("%s: EPU = %.1f (%s).", country, rows$EPU[1], rows$year_month[1]))
    }
    first <- rows[1, ]
    last <- rows[nrow(rows), ]
    pct_desc <- if (first$EPU != 0) {
      sprintf("%+.1f%%", 100 * (last$EPU - first$EPU) / abs(first$EPU))
    } else {
      "pct change undefined (starts at 0)"
    }
    diffs <- diff(rows$EPU)
    idx <- which.max(abs(diffs))
    move_desc <- sprintf(
      "biggest move: %+.1f from %s to %s",
      diffs[idx], rows$year_month[idx], rows$year_month[idx + 1]
    )
    sprintf(
      "%s: EPU %.1f (%s) -> %.1f (%s), %s; %s.",
      country, first$EPU, first$year_month, last$EPU, last$year_month, pct_desc, move_desc
    )
  }, character(1), USE.NAMES = FALSE)
  paste(blocks, collapse = "\n")
}

#' Pretty-print a slice of a result table
#'
#' @param result A data.frame with a `year_month` column.
#' @param start,end Inclusive `"YYYY-MM"` window to filter to.
#' @return The filtered data.frame, invisibly.
#' @export
print_period <- function(result, start, end) {
  filtered <- result[result$year_month >= start & result$year_month <= end, ]
  filtered <- filtered[order(filtered$country, filtered$year_month), ]
  if (nrow(filtered) == 0) {
    stop("no rows with year_month in [", start, ", ", end, "]")
  }
  print(filtered, row.names = FALSE)
  invisible(filtered)
}

#' Min/max/mean/std and the biggest month-over-month move, per country
#'
#' @param result A data.frame with `country`, `year_month`, `EPU` columns.
#' @return A data.frame with columns `country`, `min`, `max`, `mean`,
#'   `std`, `n_months`, `biggest_move`, `biggest_move_month` (the month the
#'   biggest move arrived in; `NA` for a country with a single month).
#' @export
describe <- function(result) {
  countries <- sort(unique(result$country))
  rows <- lapply(countries, function(country) {
    sub <- result[result$country == country, ]
    sub <- sub[order(sub$year_month), ]
    diffs <- if (nrow(sub) >= 2) diff(sub$EPU) else numeric(0)
    if (length(diffs) > 0) {
      idx <- which.max(abs(diffs))
      biggest_move <- diffs[idx]
      biggest_move_month <- sub$year_month[idx + 1]
    } else {
      biggest_move <- NA_real_
      biggest_move_month <- NA_character_
    }
    data.frame(
      country = country,
      min = min(sub$EPU), max = max(sub$EPU), mean = mean(sub$EPU),
      std = .population_sd(sub$EPU), n_months = nrow(sub),
      biggest_move = biggest_move, biggest_move_month = biggest_move_month,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$country), ]
}

#' Top-N rows of a result table by a column
#'
#' @param result A data.frame.
#' @param n How many rows to return (default `5`).
#' @param by Column of `result` to sort descending by (default `"EPU"`).
#' @return The top `n` rows of `result`, sorted descending by `by`
#'   (ties broken by original row order).
#' @export
top <- function(result, n = 5, by = "EPU") {
  if (!(by %in% names(result))) {
    stop("'", by, "' is not a column of result. Available: ", paste(names(result), collapse = ", "))
  }
  ord <- order(-result[[by]])
  result[ord, ][seq_len(min(n, nrow(result))), ]
}

#' Render a one-page report to an HTML or Markdown file
#'
#' @param result A data.frame with `country`, `year_month`, `EPU` columns.
#' @param path Destination file path; format is chosen by extension
#'   (`.html`, or `.md`/`.markdown`).
#' @return `path`, invisibly.
#' @export
report <- function(result, path) {
  ext <- tolower(sub(".*\\.", "", path))
  if (!(ext %in% c("html", "md", "markdown"))) {
    stop("report(): unsupported extension '.", ext, "' in ", path, " -- use .html, .md, or .markdown")
  }

  desc <- describe(result)
  summary_lines <- strsplit(summarize(result), "\n", fixed = TRUE)[[1]]

  if (ext == "html") {
    rows_html <- paste(apply(desc, 1, function(r) {
      sprintf(
        "<tr><td>%s</td><td>%.2f</td><td>%.2f</td><td>%.2f</td><td>%.2f</td><td>%s</td></tr>",
        r[["country"]], as.numeric(r[["min"]]), as.numeric(r[["max"]]),
        as.numeric(r[["mean"]]), as.numeric(r[["std"]]), r[["n_months"]]
      )
    }), collapse = "\n")
    summary_html <- paste(sprintf("<p>%s</p>", summary_lines), collapse = "\n")
    content <- paste0(
      "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><title>newsindex report</title></head><body>\n",
      "<h1>newsindex EPU Report</h1>\n",
      "<h2>Summary</h2>\n", summary_html, "\n",
      "<h2>Country statistics</h2>\n",
      "<table border=\"1\" cellpadding=\"4\" cellspacing=\"0\">\n",
      "<tr><th>Country</th><th>Min</th><th>Max</th><th>Mean</th><th>Std</th><th>Months</th></tr>\n",
      rows_html, "\n</table>\n</body></html>\n"
    )
  } else {
    rows_md <- paste(apply(desc, 1, function(r) {
      sprintf(
        "| %s | %.2f | %.2f | %.2f | %.2f | %s |",
        r[["country"]], as.numeric(r[["min"]]), as.numeric(r[["max"]]),
        as.numeric(r[["mean"]]), as.numeric(r[["std"]]), r[["n_months"]]
      )
    }), collapse = "\n")
    content <- paste0(
      "# newsindex EPU Report\n\n",
      "## Summary\n\n", paste(summary_lines, collapse = "\n\n"), "\n\n",
      "## Country statistics\n\n",
      "| Country | Min | Max | Mean | Std | Months |\n",
      "|---|---|---|---|---|---|\n",
      rows_md, "\n"
    )
  }

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(content, con, useBytes = TRUE)
  invisible(path)
}
