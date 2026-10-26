# newsindex (R)

An R package for building news-based Economic Policy Uncertainty (EPU) indices from newspaper archives, implementing the method behind the CAPE Africa EPU Index (Baker, Bloom and Davis, 2016, adapted for multilingual African press).

This is the R counterpart of [newsindex (Python)](https://github.com/CAPE-ERC/newsindex-library). Both packages implement the same methodology against the same production keyword dictionary and GDP data, so a country or regional EPU series computed in R should match the one computed in Python exactly.

## Install

```r
# install.packages("remotes")
remotes::install_github("CAPE-ERC/newsindex-r")
```

Requires R >= 4.1. Dependencies: `jsonlite`, `stringi`, `digest` (plus base `stats`/`utils`).

## Quick start

`run_index()`/`run_index_with_baseline()` wrap reading, deduplicating, counting, and scoring into a single call, for anyone who just wants an index out of an article archive without assembling the pipeline by hand:

```r
library(newsindex)

# No baseline: one window, fit and scored in-sample.
country <- run_index(
  "Scrapped_Top_Articles/", "data/eptu_country_keywords.json", start = "2020-01", end = "2020-12"
)

# With baseline: freeze 2015-2024, then score 2025 against it.
out <- run_index_with_baseline(
  "Scrapped_Top_Articles/", "data/eptu_country_keywords.json",
  windows = list(c("2025-01", "2025-12")),
  baseline_start = "2015-01", baseline_end = "2024-12"
)
baseline_save(out$baseline, "baseline_2015_2024.json")  # reuse next run: pass baseline = ... instead
country <- out$result
```

Both print progress by default (`verbose = FALSE` to silence) and accept `countries = c(...)` to read only some of the archive's country folders (default: every folder; folder names are matched case-insensitively). Neither function has a bundled default dictionary -- `dictionary_path` is always an explicit argument, since this package ships no proprietary keyword data.

These two calls are thin compositions of `read_articles()`, `dedupe_articles()`, `count_articles()`, and `compute_index()`/`fit_baseline()`+`score_windows()` -- reach for those directly (see Usage below) for anything a single call can't express.

## Usage

```r
library(newsindex)

kw <- dictionary_load(system.file("extdata", "eptu_country_keywords.json", package = "newsindex"))
arts <- read_articles("Scrapped_Top_Articles/")              # site, date, title, url, text
counts <- count_articles(dedupe_articles(arts), kw)           # country-site-month counts

baseline <- fit_baseline(counts, start = "2015-01", end = "2025-12")
baseline_save(baseline, "baseline_2015_2025.json")

country <- score_index(counts, baseline)                      # country EPU series
gdp <- monthly_gdp(system.file("extdata", "gdp_ppp_2015_2025.csv", package = "newsindex"), scale = 1000)
africa <- aggregate_regions(country, gdp, groups = AFRICA_REGIONS, baseline = baseline)
```

## Functions

R doesn't have Python's submodules, so every function is flat in the `newsindex` namespace with a short prefix instead of `ni.module.function()`. Mapping to the Python package:

| Python | R | Responsibility |
| --- | --- | --- |
| `match.normalize` | `normalize_text()` | Unicode NFKC + case-fold + quote/dash normalization |
| `match.find_terms` | `find_terms()` | distinct dictionary terms found in text |
| `match.is_epu` | `is_epu()` | Economic AND Policy AND Uncertainty classification |
| `dictionary.load` | `dictionary_load()` | load an EPTU-schema keyword dictionary |
| `dictionary.validate` | `dictionary_validate()` | empty-component / cross-component overlap audit |
| `dictionary.fingerprint` | `dictionary_fingerprint()` | stable SHA-256 hash of dictionary contents |
| `dedup.canonical_url` | `canonical_url()` | URL normalization for deduplication |
| `dedup.dedupe` | `dedupe_articles()` | drop duplicate articles within a country-month |
| `index.count` | `count_articles()` | EPU / total article counts per country-site-month |
| `index.compute_index` | `compute_index()` | no-baseline variant: fit and score one window in-sample |
| `index.fit_baseline` | `fit_baseline()` | fit frozen newspaper `sigma` and country `mu` |
| `index.score` | `score_index()` | score one window against a frozen baseline |
| `index.score_windows` | `score_windows()` | with-baseline variant: score several later windows at once |
| `Baseline.save` / `.load` | `baseline_save()` / `baseline_load()` | persist/reload a baseline as JSON |
| `aggregate.monthly_gdp` | `monthly_gdp()` | annual GDP-PPP -> monthly, log-linear interpolation |
| `aggregate.aggregate` | `aggregate_regions()` | GDP-weighted regional / Africa z-score aggregation |
| `AFRICA_REGIONS` | `AFRICA_REGIONS` | the five CAPE African regions |
| `read_articles` | `read_articles()` | read a scraped article archive into one table |
| `countries` | `countries()` | global ISO 3166-1 + UN M49 country/region reference table |
| `run_index` | `run_index()` | one-call no-baseline pipeline: read, dedupe, count, and compute an index |
| `run_index_with_baseline` | `run_index_with_baseline()` | one-call with-baseline pipeline: read, dedupe, count, fit/reuse a baseline, and score |

## Countries

`countries()` returns a global country reference table -- not just Africa, and not specific to this package's own naming -- for building your own country groupings (e.g. for `aggregate_regions()`'s `groups` argument) without compiling a list by hand:

```r
> head(countries(), 3)
         name iso2 iso3 continent      sub_region intermediate_region
1 Afghanistan   AF  AFG      Asia   Southern Asia
2     Albania   AL  ALB    Europe Southern Europe
3     Algeria   DZ  DZA    Africa Northern Africa
```

All 249 ISO 3166-1 entries (UN member states and dependencies/territories alike) are included, each with its UN M49 geographic classification. That classification is three columns, and the third is only populated for some countries:

- `continent` -- one of 5 values.
- `sub_region` -- one of 17 values.
- `intermediate_region` -- one of 7 values, set **only** for Africa and the Americas (e.g. Nigeria's `sub_region` is "Sub-Saharan Africa" and its `intermediate_region` is "Western Africa"); every other continent's `sub_region` is already the fine-grained level, so `intermediate_region` is blank (`""`) there.

The full hierarchy:

- **Africa**
  - Northern Africa
  - Sub-Saharan Africa (Eastern Africa, Middle Africa, Southern Africa, Western Africa)
- **Americas**
  - Latin America and the Caribbean (Caribbean, Central America, South America)
  - Northern America
- **Asia**
  - Central Asia
  - Eastern Asia
  - South-eastern Asia
  - Southern Asia
  - Western Asia
- **Europe**
  - Eastern Europe
  - Northern Europe
  - Southern Europe
  - Western Europe
- **Oceania**
  - Australia and New Zealand
  - Melanesia
  - Micronesia
  - Polynesia

`continent`/`sub_region`/`intermediate_region` are blank for the two ISO entries UN M49 does not classify (Antarctica; Taiwan, which is not a UN member).

`name` is ISO 3166-1's official short name, which does not always match the names used elsewhere in this package -- e.g. this table's "Congo, Democratic Republic of the" is `AFRICA_REGIONS`'s and the EPU dictionary's "Congo DRC". That's expected: `countries()` is a neutral standard reference, independent of this project's own naming, the same way a GDP source file's "Cabo Verde" needs `DEFAULT_COUNTRY_ALIASES` to line up with "Cape Verde" elsewhere.

`countries()` is purely a reference table: nothing else in this package reads it automatically. `AFRICA_REGIONS` (CAPE's own published Africa EPU grouping) and any `groups` you pass to `aggregate_regions()` yourself are unaffected by it and unchanged.

For the full 249-row table, call `countries()` or see [`inst/extdata/countries.csv`](inst/extdata/countries.csv).

## Method

- **Classify.** An article counts as EPU if its title or body contains at least one Economic, one Policy, and one Uncertainty term from its country's dictionary. A term belongs to one component only.
- **Scale.** `S_it = EPU articles_it / all articles_it` for newspaper *i* in month *t*.
- **Standardise.** `N_it = S_it / sigma_i`, where `sigma_i` is the newspaper's population standard deviation over the baseline window. The window is always chosen by the caller (the published CAPE Africa EPU index uses 2015-01 to 2025-12; the package has no hardcoded default).
- **Average and normalise.** `M_t` is the mean of `N_it` across newspapers; `EPU_t = 100 * M_t / mu`, where `mu` is the baseline mean of `M_t`.
- **Freeze.** `sigma_i`, `mu`, and the retained newspapers are saved as a JSON baseline (`baseline_save()`/`baseline_load()`); later months are scored against it and never recalibrated.
- **Aggregate.** `z_ct = (EPU_ct - m_c) / s_c`; regional `Z_rt = sum(w_ct * z_ct)` with GDP-PPP weights; rescaled to `100 + 10 * standardised Z_rt`. The Africa series applies the same steps across all countries directly -- it is not an average of the regional series.

### Two ways to compute an index

- **No baseline (`compute_index()`).** One window. `sigma`/`mu` are fit *from* that window and used to score that same window -- nothing is frozen or reused. `compute_index(counts, start = "2020-01", end = "2020-12")`.
- **With baseline (`fit_baseline()` + `score_index()` / `score_windows()`).** Two or more windows. The first window is frozen once (`fit_baseline()`); any later window (or several) is scored against that frozen baseline, never refitting it:
  ```r
  baseline <- fit_baseline(counts, start = "2015-01", end = "2024-12")
  later <- score_windows(counts, baseline, list(c("2025-01", "2025-12")))
  ```
  `score_windows()` just calls `score_index()` once per `c(start, end)` pair and tags each result with a `window` column; `score_index(counts, baseline)` alone is equivalent when you only need one later window and don't need that column.

## Examples

See [`examples/`](examples/) for a hand-labeled test dataset (`sample_articles.xlsx`, shared with the Python repo) and two R Markdown notebooks that run the full pipeline against it (`test_with_excel.Rmd`, already rendered to `.html`) and against the real Drive archive (`test_with_drive.Rmd`).

## Data

`inst/extdata/eptu_country_keywords.json` and `inst/extdata/gdp_ppp_2015_2025.csv` are the same production files bundled in the Python repo's `data/` folder (reach them via `system.file("extdata", ..., package = "newsindex")` once the package is installed).

Article archives are not bundled; `read_articles()` expects the scraper's own layout: `<root>/<country>/<newspaper>/<year-month>/articles_<newspaper>_<year-month>.csv(.gz)`, each file with `site, date, title, url, text` columns.

## Testing

```r
devtools::test()
# or, for the full package check:
devtools::check()
```

Tests mirror the Python test suite's hand-computed values exactly (same baseline-fitting and scoring example, same GDP log-linear interpolation check, same weighted-aggregation worked example), plus validation against the real bundled production dictionary.

## License

MIT. See [LICENSE.md](LICENSE.md).
