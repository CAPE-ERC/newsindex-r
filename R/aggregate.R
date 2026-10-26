#' Country z-scores, weighting, and regional / Africa aggregation
#'
#' Implements the four computation steps from the CAPE Africa EPU
#' methodology: standardise each country's index against its own baseline,
#' weight by within-group share (GDP-PPP by default, or any table or
#' custom weighting function -- see [aggregate_regions()]), take the
#' weighted average of country z-scores, and rescale the result to a
#' baseline mean of 100 and standard deviation of 10. The Africa aggregate
#' applies the same four steps across all countries directly; it is not an
#' average of the regional series.
#'
#' @name aggregate
NULL

#' The five CAPE African regions
#'
#' As published in the Africa EPU Index methodology (Table 8). Country
#' names match the keys used in the production `eptu_country_keywords.json`
#' dictionary.
#'
#' @format A named list of character vectors, one per region.
#' @export
AFRICA_REGIONS <- list(
  Central = c(
    "Cameroon", "Central African Republic", "Chad", "Congo", "Congo DRC",
    "Equatorial Guinea", "Gabon"
  ),
  East = c(
    "Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya",
    "Rwanda", "Seychelles", "Somalia", "South Sudan", "Sudan", "Tanzania",
    "Uganda"
  ),
  North = c("Algeria", "Egypt", "Libya", "Mauritania", "Morocco", "Tunisia"),
  Southern = c(
    "Angola", "Botswana", "Eswatini", "Lesotho", "Madagascar", "Malawi",
    "Mauritius", "Mozambique", "Namibia", "Sao Tome and Principe",
    "South Africa", "Zambia", "Zimbabwe"
  ),
  West = c(
    "Benin", "Burkina-Faso", "Cape Verde", "Gambia", "Ghana", "Guinea",
    "Guinea-Bissau", "Ivory Coast", "Liberia", "Mali", "Niger", "Nigeria",
    "Senegal", "Sierra Leone", "Togo"
  )
)

#' Country-name aliases between common GDP source spellings and the names
#' used by [AFRICA_REGIONS] and the EPU keyword dictionary
#'
#' @format A named character vector mapping source name to canonical name.
#' @export
DEFAULT_COUNTRY_ALIASES <- c(
  "Cabo Verde" = "Cape Verde",
  "Burkina Faso" = "Burkina-Faso",
  "Congo, The Democratic Republic of the" = "Congo DRC",
  "Democratic Republic of the Congo" = "Congo DRC",
  "Congo, Dem. Rep." = "Congo DRC",
  "Congo, Rep." = "Congo",
  "Cote d'Ivoire" = "Ivory Coast",
  "C\u00f4te d'Ivoire" = "Ivory Coast",
  "Eswatini (Swaziland)" = "Eswatini",
  "Sao Tome and Principe" = "Sao Tome and Principe",
  "The Gambia" = "Gambia"
)

#' Interpolate annual GDP-PPP (or any other annual-by-country metric) to
#' monthly values
#'
#' Each annual value is placed at 1 January of its year and interpolated
#' linearly in logarithms through the next year's figure. After a country's
#' last annual figure, the value is held flat (carried forward), optionally
#' through `end` (a `"YYYY-MM"` string).
#'
#' @param source Either a path to a CSV with country, year, and value
#'   columns, or an already-loaded data.frame with those columns.
#' @param country_col,year_col,value_col Column names in `source`.
#' @param scale Multiplies every value after it is read, e.g. `scale = 1000`
#'   to convert a billions-denominated source file to millions.
#' @param end Optional `"YYYY-MM"` string to carry the last year's value
#'   forward through.
#' @param aliases Named character vector mapping source country names to
#'   canonical names (default [DEFAULT_COUNTRY_ALIASES]).
#' @return A data.frame with columns `country`, `year_month`, `gdp`.
#' @export
monthly_gdp <- function(source, country_col = "Country", year_col = "Year",
                         value_col = "GDP_Billions_USD", scale = 1,
                         end = NULL, aliases = DEFAULT_COUNTRY_ALIASES) {
  raw <- if (is.data.frame(source)) {
    source
  } else {
    utils::read.csv(source, stringsAsFactors = FALSE, check.names = FALSE)
  }
  country_values <- raw[[country_col]]
  mapped <- aliases[country_values]
  raw[[country_col]] <- ifelse(is.na(mapped), country_values, mapped)

  rows <- list()
  for (country in sort(unique(raw[[country_col]]))) {
    sub <- raw[raw[[country_col]] == country, ]
    sub <- sub[order(sub[[year_col]]), ]
    years <- as.integer(sub[[year_col]])
    values <- as.numeric(sub[[value_col]]) * scale

    for (i in seq_along(years)) {
      year <- years[i]
      value <- values[i]
      has_next <- i < length(years) && years[i + 1] != year
      if (has_next) {
        next_value <- values[i + 1]
        for (m in 0:11) {
          frac <- m / 12
          interpolated <- if (value > 0 && next_value > 0) {
            exp(log(value) + frac * (log(next_value) - log(value)))
          } else {
            value
          }
          rows[[length(rows) + 1]] <- data.frame(
            country = country, year_month = sprintf("%d-%02d", year, m + 1),
            gdp = interpolated, stringsAsFactors = FALSE
          )
        }
      } else if (i == length(years)) {
        for (m in 0:11) {
          rows[[length(rows) + 1]] <- data.frame(
            country = country, year_month = sprintf("%d-%02d", year, m + 1),
            gdp = value, stringsAsFactors = FALSE
          )
        }
        if (!is.null(end)) {
          carry_year <- year + 1
          while (sprintf("%d-01", carry_year) <= end) {
            for (m in 0:11) {
              year_month <- sprintf("%d-%02d", carry_year, m + 1)
              if (year_month > end) break
              rows[[length(rows) + 1]] <- data.frame(
                country = country, year_month = year_month, gdp = value,
                stringsAsFactors = FALSE
              )
            }
            carry_year <- carry_year + 1
          }
        }
      }
    }
  }

  result <- do.call(rbind, rows)
  result[order(result$country, result$year_month), ]
}

.population_stats <- function(x) c(mean = mean(x), sd = .population_sd(x))

#' Weighted regional (and, by default, Africa-wide) EPU series
#'
#' @param country_index A data.frame with `country`, `year_month`, and
#'   `EPU` columns (as produced by [score_index()]).
#' @param weights Controls how countries are combined within each group,
#'   and is either:
#'   * A data.frame with `country`, `year_month`, and a weight-value
#'     column (`weight_col`, default `"gdp"` -- the column name produced
#'     by [monthly_gdp()]). Only countries present in this table are
#'     included. Within each group and month, countries are weighted by
#'     their share of the group's total.
#'   * A function `function(month_rows, group_name)`, called once per
#'     group per month with that month's rows for the group (columns
#'     include `country` and `z`) and the group's name, returning a
#'     non-negative weight per row (same length and order as
#'     `month_rows`). Weights don't need to be pre-normalised -- they are
#'     divided by their own sum -- so e.g. equal weighting is
#'     `function(rows, group) rep(1, nrow(rows))`.
#' @param groups A named list of character vectors (default
#'   [AFRICA_REGIONS]).
#' @param weight_col Column in `weights` holding the weight value, when
#'   `weights` is a data.frame (default `"gdp"`).
#' @param baseline Optional `newsindex_baseline` used to score
#'   `country_index`; if given, its `start`/`end` override
#'   `baseline_start`/`baseline_end`.
#' @param baseline_start,baseline_end `"YYYY-MM"` strings bounding the
#'   baseline window used for both the country z-score and the regional
#'   rescaling.
#' @param include_africa If `TRUE` (default), also compute an `"Africa"`
#'   group spanning the union of all countries in `groups`.
#' @return A data.frame with columns `group`, `year_month`, `z`, `index`,
#'   `n_countries`, `weight_covered`.
#' @export
aggregate_regions <- function(country_index, weights, groups = AFRICA_REGIONS,
                               weight_col = "gdp", baseline = NULL,
                               baseline_start = "2015-01", baseline_end = "2025-12",
                               include_africa = TRUE) {
  if (!is.null(baseline)) {
    baseline_start <- baseline$start
    baseline_end <- baseline$end
  }

  merged <- if (is.function(weights)) {
    country_index
  } else {
    merge(country_index, weights, by = c("country", "year_month"))
  }
  window <- merged[merged$year_month >= baseline_start & merged$year_month <= baseline_end, ]

  country_stats <- list()
  for (country in unique(window$country)) {
    values <- window$EPU[window$country == country]
    if (length(values) >= 2) {
      country_stats[[country]] <- .population_stats(values)
    }
  }

  merged <- merged[merged$country %in% names(country_stats), ]
  merged$m_c <- vapply(merged$country, function(c) country_stats[[c]]["mean"], numeric(1))
  merged$s_c <- vapply(merged$country, function(c) country_stats[[c]]["sd"], numeric(1))
  merged <- merged[merged$s_c > 0, ]
  merged$z <- (merged$EPU - merged$m_c) / merged$s_c

  all_groups <- groups
  if (include_africa) {
    all_groups[["Africa"]] <- sort(unique(unlist(groups, use.names = FALSE)))
  }

  group_rows <- list()
  for (group_name in names(all_groups)) {
    group_data <- merged[merged$country %in% all_groups[[group_name]], ]
    if (nrow(group_data) == 0) next
    for (year_month in sort(unique(group_data$year_month))) {
      month_rows <- group_data[group_data$year_month == year_month, ]
      raw_weight <- if (is.function(weights)) {
        weights(month_rows, group_name)
      } else {
        month_rows[[weight_col]]
      }
      total_weight <- sum(raw_weight)
      if (total_weight <= 0) next
      weight <- raw_weight / total_weight
      group_rows[[length(group_rows) + 1]] <- data.frame(
        group = group_name, year_month = year_month,
        Z = sum(weight * month_rows$z),
        n_countries = length(unique(month_rows$country)),
        weight_covered = total_weight, stringsAsFactors = FALSE
      )
    }
  }

  if (length(group_rows) == 0) {
    return(data.frame(
      group = character(), year_month = character(), z = numeric(),
      index = numeric(), n_countries = integer(), weight_covered = numeric()
    ))
  }
  regional <- do.call(rbind, group_rows)

  output_rows <- list()
  for (group_name in unique(regional$group)) {
    rows <- regional[regional$group == group_name, ]
    window_rows <- rows[rows$year_month >= baseline_start & rows$year_month <= baseline_end, ]
    if (nrow(window_rows) < 2) next
    stats_r <- .population_stats(window_rows$Z)
    m_r <- stats_r["mean"]
    s_r <- stats_r["sd"]
    if (s_r <= 0) next
    output_rows[[length(output_rows) + 1]] <- data.frame(
      group = rows$group, year_month = rows$year_month, z = rows$Z,
      index = 100 + 10 * (rows$Z - m_r) / s_r,
      n_countries = rows$n_countries, weight_covered = rows$weight_covered,
      stringsAsFactors = FALSE
    )
  }
  if (length(output_rows) == 0) {
    return(data.frame(
      group = character(), year_month = character(), z = numeric(),
      index = numeric(), n_countries = integer(), weight_covered = numeric()
    ))
  }
  result <- do.call(rbind, output_rows)
  result[order(result$group, result$year_month), ]
}
