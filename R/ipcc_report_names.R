# IPCC report abbreviation mapping
# =============================================================================

.ipcc_report_name_lookup <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)

    path <- ""
    if (exists(".pkg_file", mode = "function")) {
      path <- .pkg_file("extdata", "ipcc_report_names.csv")
    }
    if (!nzchar(path)) {
      path <- file.path(getwd(), "inst", "extdata", "ipcc_report_names.csv")
    }
    if (!file.exists(path)) {
      cache <<- stats::setNames(character(0), character(0))
      return(cache)
    }

    df <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0 || !all(c("abbreviation", "long_name") %in% names(df))) {
      cache <<- stats::setNames(character(0), character(0))
      return(cache)
    }

    abbr <- trimws(as.character(df$abbreviation))
    long <- trimws(as.character(df$long_name))
    keep <- nzchar(abbr) & nzchar(long) & !is.na(abbr) & !is.na(long)
    map <- stats::setNames(long[keep], abbr[keep])

    cache <<- map
    cache
  }
})

.expand_ipcc_report_name <- function(x) {
  key <- trimws(as.character(x))
  if (!nzchar(key) || is.na(key)) return(key)

  map <- .ipcc_report_name_lookup()
  val <- unname(map[key])
  if (length(val) == 0 || is.na(val) || !nzchar(val)) key else as.character(val)
}
