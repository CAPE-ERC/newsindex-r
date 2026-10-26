# newsindex (R)

An R package for building news-based Economic Policy Uncertainty (EPU) indices from newspaper archives, implementing the method behind the CAPE Africa EPU Index (Baker, Bloom and Davis, 2016, adapted for multilingual African press).

This is the R counterpart of [newsindex (Python)](https://github.com/CAPE-ERC/newsindex-library). Both packages implement the same methodology against the same production keyword dictionary and GDP data, so a country or regional EPU series computed in R should match the one computed in Python exactly.

## Install

```r
# install.packages("remotes")
remotes::install_github("CAPE-ERC/newsindex-r")
```

Requires R >= 4.1. Dependencies: `jsonlite`, `stringi`, `digest` (plus base `stats`/`utils`).

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
