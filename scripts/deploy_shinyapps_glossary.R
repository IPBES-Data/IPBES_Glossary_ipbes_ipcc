# Deploy helper for shinyapps.io (Glossary Explorer app)
#
# Usage:
#   Rscript scripts/deploy_shinyapps_glossary.R
#
# Optional environment variables:
#   SHINYAPPS_ACCOUNT     (default: rmkrug)
#   SHINYAPPS_APP_NAME    (explicit override target name)
#   DEPLOY_BRANCH         (defaults to current git branch)
#   DEPLOY_DRY_RUN        (1/true/yes to print target only)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

source(file.path("scripts", "deploy_helpers.R"))

account <- trimws(Sys.getenv("SHINYAPPS_ACCOUNT", "rmkrug"))
base_app_name <- "glossary-ipbes-ipcc-explorer"
explicit_name <- trimws(Sys.getenv("SHINYAPPS_APP_NAME", ""))
branch <- resolve_deploy_branch()
app_name <- resolve_target_name(base_app_name, branch, explicit_name)

if (!nzchar(account)) {
  stop("Missing shinyapps.io account. Set SHINYAPPS_ACCOUNT.")
}

required_cache_files <- c(
  "inst/extdata/ipbes_glossary.csv",
  "inst/extdata/ipcc_glossary.csv",
  "inst/extdata/ipcc_glossary_multilingual.csv",
  "inst/extdata/merged_glossary_cache.rds",
  "inst/extdata/hierarchy_edges_cache.rds"
)
ensure_required_cache_files(required_cache_files)

print_deploy_target("shinyapps", branch, base_app_name, app_name, explicit_name)
cat(sprintf("[shinyapps] account=%s\n", account))

if (is_deploy_dry_run()) {
  cat("[shinyapps] DEPLOY_DRY_RUN=1 -> skipping deploy call.\n")
  quit(save = "no", status = 0)
}

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app_glossary.R",
  appName       = app_name,
  account       = account,
  forceUpdate   = TRUE
)
