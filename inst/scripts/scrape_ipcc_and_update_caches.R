#!/usr/bin/env Rscript

# Scrape the live IPCC glossary into inst/extdata/ipcc_glossary.csv, then
# rebuild all bundled caches.
#
# Run from the repo root:
#   Rscript inst/scripts/scrape_ipcc_and_update_caches.R

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

find_repo_root <- function() {
  full_args <- commandArgs(trailingOnly = FALSE)
  file_arg  <- grep("^--file=", full_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- normalizePath(
      dirname(sub("^--file=", "", file_arg[[1]])),
      winslash = "/", mustWork = FALSE
    )
    root <- dirname(dirname(script_dir))
    if (dir.exists(file.path(root, "inst", "extdata"))) return(root)
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(wd, "inst", "extdata"))) return(wd)
  stop("Cannot locate repo root. Run from the repo root or via Rscript inst/scripts/scrape_ipcc_and_update_caches.R")
}

repo_root   <- find_repo_root()
extdata_dir <- file.path(repo_root, "inst", "extdata")
ipcc_dest   <- file.path(extdata_dir, "ipcc_glossary.csv")
r_dir       <- file.path(repo_root, "glossary", "R")

cache_update_script <- file.path(repo_root, "inst", "scripts", "update_bundled_caches.R")

if (!dir.exists(extdata_dir))       stop("Missing extdata directory: ", extdata_dir)
if (!file.exists(cache_update_script)) stop("Missing script: ", cache_update_script)

cat("Repo root:", repo_root, "\n")
cat("Scraping IPCC glossary from live endpoint...\n")

for (f in c("utils.R", "data_ipcc.R", "ipcc_report_names.R")) {
  source(file.path(r_dir, f))
}

tmp_dir <- tempfile("ipcc_scrape_")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

scraped_path <- scrape_ipcc(cache_dir = tmp_dir)
if (!file.exists(scraped_path)) {
  stop("Scrape finished but output file not found: ", scraped_path)
}

ok <- file.copy(scraped_path, ipcc_dest, overwrite = TRUE)
if (!isTRUE(ok)) stop("Failed to copy scraped file to: ", ipcc_dest)
cat("Updated:", ipcc_dest, "\n")

cat("Rebuilding bundled caches...\n")
rscript  <- file.path(R.home("bin"), "Rscript")
cmd_out  <- system2(rscript, args = c(cache_update_script, "--force"),
                    stdout = TRUE, stderr = TRUE)
status   <- attr(cmd_out, "status") %||% 0L

if (length(cmd_out) > 0) cat(paste(cmd_out, collapse = "\n"), "\n")
if (!identical(as.integer(status), 0L)) {
  stop("Cache update script failed with status: ", status)
}

cat("Done.\n")
