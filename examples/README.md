# Examples

R counterpart of the examples in the Python
[`newsindex-library`](https://github.com/CAPE-ERC/newsindex-library/tree/main/examples)
repo -- same dataset, same hand-computed worked example, so results match exactly
between the two languages.

| File | What it is |
| --- | --- |
| [`eptu_country_keywords.json`](eptu_country_keywords.json) | Convenience copy of the production dictionary (same file as [`../inst/extdata/eptu_country_keywords.json`](../inst/extdata/eptu_country_keywords.json)) so this folder is self-contained. The notebooks below still load the canonical package copy via `system.file()`; if that one is ever regenerated, copy it here again to keep the two in sync. |
| [`sample_articles.xlsx`](sample_articles.xlsx) | The same 24 hand-labeled articles used by the Python repo's example (2 countries x 2 newspapers x 2 months x 3 articles), with `expected_is_epu` verified against the real production dictionary. |
| [`test_with_excel.Rmd`](test_with_excel.Rmd) / [`test_with_excel.html`](test_with_excel.html) | Runs `sample_articles.xlsx` through classification, counting, baseline fitting, scoring, baseline save/load, and aggregation -- and `stopifnot()`-asserts every result against hand-computed values. Self-contained; needs no external data. The `.html` is the already-rendered output, so you can read the results without re-running it. |
| [`test_with_drive.Rmd`](test_with_drive.Rmd) | Runs the same pipeline against the real scraped-article archive in Google Drive (`Shak_Scrapped_Top_Articles`). Defaults to a small country slice and to `eval = FALSE` (set `DRIVE_ROOT` and knit with `eval = TRUE`, or run chunks interactively) -- widen `COUNTRIES` deliberately once you know the runtime on your connection. |

To re-knit `test_with_excel.Rmd` yourself:

```r
install.packages("readxl")
rmarkdown::render("examples/test_with_excel.Rmd")
```
