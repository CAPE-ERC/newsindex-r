.write_pipeline_dictionary <- function() {
  dict_path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(countries = list(
    Nigeria = list(
      languages = list("en"), ECONOMY_TERMS = list("fiscal", "inflation"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertainty")
    ),
    Kenya = list(
      languages = list("en"), ECONOMY_TERMS = list("fiscal", "inflation"),
      POLICY_TERMS = list("government"), UNCERTAINTY_TERMS = list("uncertain")
    )
  )), auto_unbox = TRUE), dict_path)
  dict_path
}

.write_pipeline_articles <- function(path, rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- paste(c("site,date,title,url,text", rows), collapse = "\n")
  con <- gzfile(path, "w")
  writeLines(body, con)
  close(con)
}

.build_pipeline_archive <- function(root) {
  months <- list(
    list(month = "2020-01", epu = 1, plain = 3),
    list(month = "2020-02", epu = 2, plain = 2),
    list(month = "2020-03", epu = 1, plain = 3)
  )
  for (m in months) {
    epu_rows <- vapply(seq_len(m$epu), function(i) {
      sprintf(
        'The Punch,%s-01,"Government fiscal policy and inflation create uncertainty",https://example.com/nigeria/%s/%d,body',
        m$month, m$month, i
      )
    }, character(1))
    plain_rows <- vapply(seq_len(m$plain), function(i) {
      sprintf(
        'The Punch,%s-02,"Local football result %d",https://example.com/nigeria/%s/p%d,body',
        m$month, i, m$month, i
      )
    }, character(1))
    .write_pipeline_articles(
      file.path(root, "Nigeria", "The Punch", m$month, sprintf("articles_The Punch_%s.csv.gz", m$month)),
      c(epu_rows, plain_rows)
    )
  }

  months2 <- list(
    list(month = "2020-01", epu = 2, plain = 2),
    list(month = "2020-02", epu = 1, plain = 3),
    list(month = "2020-03", epu = 2, plain = 2)
  )
  for (m in months2) {
    epu_rows <- vapply(seq_len(m$epu), function(i) {
      sprintf(
        'Daily Nation,%s-01,"Government fiscal policy and inflation create uncertain economy",https://example.com/kenya/%s/%d,body',
        m$month, m$month, i
      )
    }, character(1))
    plain_rows <- vapply(seq_len(m$plain), function(i) {
      sprintf(
        'Daily Nation,%s-02,"Weather update %d",https://example.com/kenya/%s/p%d,body',
        m$month, i, m$month, i
      )
    }, character(1))
    .write_pipeline_articles(
      file.path(root, "Kenya", "Daily Nation", m$month, sprintf("articles_Daily Nation_%s.csv.gz", m$month)),
      c(epu_rows, plain_rows)
    )
  }
}
