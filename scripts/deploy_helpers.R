# Shared deployment helpers for branch-safe publish targets.

.deploy_env_true <- function(name, default = FALSE) {
  raw <- trimws(Sys.getenv(name, if (default) "1" else "0"))
  tolower(raw) %in% c("1", "true", "yes", "y", "on")
}

.detect_git_branch <- function() {
  out <- tryCatch(
    system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(out) == 0) return(NA_character_)
  branch <- trimws(out[[1]])
  if (!nzchar(branch) || identical(branch, "HEAD")) return(NA_character_)
  branch
}

resolve_deploy_branch <- function() {
  env_branch <- trimws(Sys.getenv("DEPLOY_BRANCH", ""))
  if (nzchar(env_branch)) return(env_branch)
  branch <- .detect_git_branch()
  if (is.na(branch)) "unknown" else branch
}

is_prod_branch <- function(branch) {
  b <- tolower(trimws(branch))
  if (!nzchar(b)) return(FALSE)
  identical(b, "main") || identical(b, "master") || grepl("^release([/-].*)?$", b)
}

sanitize_branch_suffix <- function(branch) {
  b <- tolower(trimws(branch))
  if (!nzchar(b)) return("unknown")
  b <- gsub("[^a-z0-9]+", "-", b)
  b <- gsub("-+", "-", b)
  b <- gsub("^-|-$", "", b)
  if (!nzchar(b)) "unknown" else b
}

resolve_target_name <- function(base_name, branch, explicit_name = "") {
  explicit_name <- trimws(explicit_name)
  if (nzchar(explicit_name)) return(explicit_name)
  if (is_prod_branch(branch)) return(base_name)
  paste0(base_name, "-", sanitize_branch_suffix(branch))
}

resolve_known_prod_guids <- function(extra = character()) {
  raw <- trimws(Sys.getenv("CONNECT_PROD_GUIDS", ""))
  env_vals <- character(0)
  if (nzchar(raw)) {
    env_vals <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  }
  vals <- unique(c(trimws(extra), env_vals))
  vals[nzchar(vals)]
}

guard_connect_guid <- function(connect_guid, known_prod_guids, branch, allow_prod_overwrite = FALSE) {
  connect_guid <- trimws(connect_guid)
  if (!nzchar(connect_guid)) return(invisible(TRUE))

  if (!is_prod_branch(branch) && !allow_prod_overwrite && connect_guid %in% known_prod_guids) {
    stop(
      "Refusing deployment: CONNECT_GUID targets known production content on non-prod branch ('",
      branch,
      "'). Set ALLOW_PROD_OVERWRITE=1 to override intentionally."
    )
  }

  invisible(TRUE)
}

print_deploy_target <- function(provider, branch, base_name, resolved_name, explicit_name = "") {
  cat(sprintf("[%s] branch=%s\n", provider, branch))
  cat(sprintf("[%s] base_name=%s\n", provider, base_name))
  cat(sprintf("[%s] target_name=%s\n", provider, resolved_name))
  if (nzchar(trimws(explicit_name))) {
    cat(sprintf("[%s] target_name_source=explicit override\n", provider))
  }
}

ensure_required_cache_files <- function(files) {
  missing_cache_files <- files[!file.exists(files)]
  if (length(missing_cache_files) > 0) {
    stop(
      "Missing bundled cache/snapshot files. Run data-raw/prepare_data.R first:\n",
      paste0(" - ", missing_cache_files, collapse = "\n")
    )
  }
}

is_deploy_dry_run <- function() {
  .deploy_env_true("DEPLOY_DRY_RUN", default = FALSE)
}

allow_prod_overwrite <- function() {
  .deploy_env_true("ALLOW_PROD_OVERWRITE", default = FALSE)
}
