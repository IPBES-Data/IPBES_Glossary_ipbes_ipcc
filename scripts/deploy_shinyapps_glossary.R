# Deploy helper for shinyapps.io (Glossary Explorer app)
#
# Usage:
#   Rscript scripts/deploy_shinyapps_glossary.R
#
# Optional environment variables:
#   SHINYAPPS_ACCOUNT     (default: rmkrug)
#   SHINYAPPS_APP_NAME    (default: glossary-ipbes-ipcc-explorer)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

account <- trimws(Sys.getenv("SHINYAPPS_ACCOUNT", "rmkrug"))
app_name <- trimws(Sys.getenv("SHINYAPPS_APP_NAME", "glossary-ipbes-ipcc-explorer"))

if (!nzchar(account)) {
  stop("Missing shinyapps.io account. Set SHINYAPPS_ACCOUNT.")
}
if (!nzchar(app_name)) {
  stop("Missing app name. Set SHINYAPPS_APP_NAME.")
}

required_cache_files <- c(
  "inst/extdata/ipbes_glossary.csv",
  "inst/extdata/ipcc_glossary.csv",
  "inst/extdata/merged_glossary_cache.rds"
)

missing_cache_files <- required_cache_files[!file.exists(required_cache_files)]
if (length(missing_cache_files) > 0) {
  stop(
    "Missing bundled cache/snapshot files. Run data-raw/prepare_data.R first:\n",
    paste0(" - ", missing_cache_files, collapse = "\n")
  )
}

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app_glossary.R",
  appName       = app_name,
  account       = account,
  forceUpdate   = TRUE
)
