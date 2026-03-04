# Deploy helper for shinyapps.io
#
# Usage:
#   SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
#   Rscript scripts/deploy_shinyapps.R
#
# Optional:
#   SHINYAPPS_APP_NAME=glossary-ipbes-ipcc

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

account <- "rmkrug"
app_name <- "glossary-ipbes-ipcc"

if (!nzchar(account)) {
  stop(
    "Missing shinyapps.io credentials. Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET."
  )
}

required_cache_files <- c(
  "inst/extdata/ipbes_glossary.csv",
  "inst/extdata/ipcc_glossary.csv",
  "inst/extdata/merged_glossary_cache.rds",
  "inst/extdata/hierarchy_edges_cache.rds"
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
  appPrimaryDoc = "app.R",
  appName       = app_name,
  account       = account,
  forceUpdate   = TRUE
)
