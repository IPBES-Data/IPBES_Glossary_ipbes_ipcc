# Deploy helper for Posit Connect / Connect Cloud (comparison app)
#
# Required environment variables:
#   CONNECT_SERVER          (e.g. https://<tenant>.share.connect.posit.cloud)
#   CONNECT_API_KEY
#
# Optional environment variables:
#   CONNECT_SERVER_NAME     (default: posit-connect)
#   CONNECT_ACCOUNT         (auto-detected if omitted)
#   CONNECT_CONTENT_NAME    (explicit override target name)
#   CONNECT_GUID            (existing content id/guid to update)
#   CONNECT_PROD_GUID       (known production guid for guard)
#   CONNECT_PROD_GUIDS      (comma-separated known production guids)
#   DEPLOY_BRANCH           (defaults to current git branch)
#   DEPLOY_DRY_RUN          (1/true/yes to print target only)
#   ALLOW_PROD_OVERWRITE    (1/true/yes to override GUID guard)

source(file.path("scripts", "deploy_helpers.R"))

base_name <- "glossary-ipbes-ipcc"
branch <- resolve_deploy_branch()
explicit_name <- trimws(Sys.getenv("CONNECT_CONTENT_NAME", ""))
content_name <- resolve_target_name(base_name, branch, explicit_name)

connect_server <- trimws(Sys.getenv("CONNECT_SERVER", ""))
connect_server_name <- trimws(Sys.getenv("CONNECT_SERVER_NAME", "posit-connect"))
connect_api_key <- trimws(Sys.getenv("CONNECT_API_KEY", ""))
connect_account <- trimws(Sys.getenv("CONNECT_ACCOUNT", ""))
connect_guid <- trimws(Sys.getenv("CONNECT_GUID", ""))

known_prod_guids <- resolve_known_prod_guids(
  c(trimws(Sys.getenv("CONNECT_PROD_GUID", "")), trimws(Sys.getenv("CONNECT_PROD_GUID_COMPARE", "")))
)

if (!nzchar(connect_server)) stop("Missing CONNECT_SERVER.")
if (!nzchar(connect_api_key)) stop("Missing CONNECT_API_KEY.")
if (!nzchar(connect_server_name)) stop("Missing CONNECT_SERVER_NAME.")

guard_connect_guid(
  connect_guid = connect_guid,
  known_prod_guids = known_prod_guids,
  branch = branch,
  allow_prod_overwrite = allow_prod_overwrite()
)

required_cache_files <- c(
  "inst/extdata/ipbes_glossary.csv",
  "inst/extdata/ipcc_glossary.csv",
  "inst/extdata/ipcc_glossary_multilingual.csv",
  "inst/extdata/merged_glossary_cache.rds",
  "inst/extdata/hierarchy_edges_cache.rds"
)
ensure_required_cache_files(required_cache_files)

print_deploy_target("connect", branch, base_name, content_name, explicit_name)
cat(sprintf("[connect] server=%s\n", connect_server))
cat(sprintf("[connect] server_name=%s\n", connect_server_name))
if (nzchar(connect_guid)) cat(sprintf("[connect] guid=%s\n", connect_guid))

if (is_deploy_dry_run()) {
  cat("[connect] DEPLOY_DRY_RUN=1 -> skipping deploy call.\n")
  quit(save = "no", status = 0)
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

tryCatch(
  rsconnect::addConnectServer(url = connect_server, name = connect_server_name, quiet = TRUE),
  error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already|exists", msg, ignore.case = TRUE)) stop(e)
  }
)

user_info <- rsconnect::connectApiUser(
  server = connect_server_name,
  apiKey = connect_api_key,
  quiet = TRUE
)

if (!nzchar(connect_account)) {
  if (is.list(user_info) && !is.null(user_info$username) && nzchar(user_info$username)) {
    connect_account <- user_info$username
  } else if (is.list(user_info) && !is.null(user_info$name) && nzchar(user_info$name)) {
    connect_account <- user_info$name
  }
}

if (!nzchar(connect_account)) {
  accts <- tryCatch(rsconnect::accounts(server = connect_server_name), error = function(e) NULL)
  if (!is.null(accts) && nrow(accts) > 0 && "name" %in% names(accts)) {
    connect_account <- accts$name[[1]]
  }
}

if (!nzchar(connect_account)) {
  stop("Could not determine Connect account. Set CONNECT_ACCOUNT explicitly.")
}

cat(sprintf("[connect] account=%s\n", connect_account))

deploy_args <- list(
  appDir = ".",
  appPrimaryDoc = "app_compare.R",
  appName = content_name,
  appTitle = content_name,
  account = connect_account,
  server = connect_server_name,
  forceUpdate = TRUE
)
if (nzchar(connect_guid)) deploy_args$appId <- connect_guid

do.call(rsconnect::deployApp, deploy_args)
