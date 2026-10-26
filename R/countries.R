#' Global country reference: ISO 3166-1 codes and UN M49 regions
#'
#' A pure reference table, not consumed by any other function in this
#' package. It exists so a user building their own country groupings (for
#' [aggregate_regions()] or anything else) has a standard, verified list of
#' countries, codes, and regions to start from, without compiling one by
#' hand.
#'
#' The 249 rows are every ISO 3166-1 entry (UN member states and
#' dependencies/territories alike), each carrying its UN M49 geographic
#' classification. That classification is three levels, and the third
#' level is only defined for some countries: `continent` (5 values:
#' Africa, Americas, Asia, Europe, Oceania), `sub_region` (17 values), and
#' `intermediate_region` (7 values, set only for Africa and the Americas --
#' e.g. Nigeria's `sub_region` is "Sub-Saharan Africa" and its
#' `intermediate_region` is "Western Africa"; a European country has no
#' `intermediate_region` at all because its `sub_region` is already the
#' fine-grained level). `continent`/`sub_region`/`intermediate_region` are
#' blank (`""`) for the two ISO entries the UN M49 scheme does not classify
#' (Antarctica; Taiwan, which is not a UN member).
#'
#' `name` is ISO 3166-1's official short name, which does not always match
#' the names used elsewhere in this package (e.g. this table's "Congo,
#' Democratic Republic of the" is "Congo DRC" in [AFRICA_REGIONS] and the
#' EPU dictionary) -- the same kind of mismatch [DEFAULT_COUNTRY_ALIASES]
#' already bridges for GDP source files.
#'
#' @param path Optional override; defaults to the table bundled with this
#'   package.
#' @return A data.frame with columns `name`, `iso2`, `iso3`, `continent`,
#'   `sub_region`, `intermediate_region`, one row per ISO 3166-1 entry.
#' @export
countries <- function(path = NULL) {
  columns <- c("name", "iso2", "iso3", "continent", "sub_region", "intermediate_region")
  if (is.null(path)) {
    path <- system.file("extdata", "countries.csv", package = "newsindex")
    if (!nzchar(path)) {
      stop("countries.csv not found in the installed package")
    }
  }
  # na.strings must be disabled: Namibia's ISO 3166-1 alpha-2 code is
  # literally "NA", which read.csv's default na.strings would otherwise
  # silently turn into a missing value.
  table <- utils::read.csv(
    path,
    colClasses = "character", na.strings = character(0), stringsAsFactors = FALSE
  )
  table[, columns]
}
