# scripts/write_manifests.R
# Regenerate manifest.json in compare/ and glossary/ for Posit Connect
# git-backed deployment.
#
# Run from the repo root:
#   Rscript scripts/write_manifests.R
#
# rsconnect requires the renv library to be on the search path so it can
# verify packages match the lockfile.  This script locates the per-app renv
# library automatically.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect first: install.packages('rsconnect')")
}

renv_lib_subpath <- file.path(
  "renv", "library",
  paste0(tolower(Sys.info()[["sysname"]]),
         if (.Machine$sizeof.pointer == 8) "" else ""),
  paste0("R-", getRversion()[1, 1:2]),
  R.version$arch
)

write_manifest <- function(app_dir) {
  lib <- file.path(app_dir, renv_lib_subpath)
  if (!dir.exists(lib)) {
    # Fallback: let renv resolve its own library path
    old_wd <- setwd(app_dir)
    on.exit(setwd(old_wd), add = TRUE)
    lib <- tryCatch(renv::paths$library(), error = function(e) "")
    setwd(old_wd)
  }
  old_paths <- .libPaths()
  if (nzchar(lib) && dir.exists(lib)) {
    .libPaths(c(lib, old_paths))
    on.exit(.libPaths(old_paths), add = TRUE)
  }
  rsconnect::writeManifest(appDir = app_dir, appPrimaryDoc = "app.R")
  cat(sprintf("Written: %s/manifest.json\n", app_dir))
}

write_manifest("glossary")
write_manifest("compare")
