#!/usr/bin/env Rscript

# Scrape the live IPCC glossary into inst/extdata/ipcc_glossary.csv, then
# rebuild bundled caches.
#
# Usage:
#   Rscript inst/scripts/scrape_ipcc_and_update_caches.R

find_package_root <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    desc <- file.path(dir, "DESCRIPTION")
    if (file.exists(desc)) {
      dcf <- tryCatch(read.dcf(desc), error = function(e) NULL)
      if (!is.null(dcf) && "Package" %in% colnames(dcf)) {
        pkg <- unname(trimws(dcf[1, "Package"]))
        if (identical(pkg, "glossary.ipbes.ipcc")) return(dir)
      }
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  stop("Could not find package root (DESCRIPTION for glossary.ipbes.ipcc).")
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

script_path <- {
  full_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", full_args, value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)
  } else {
    ""
  }
}

repo_root <- tryCatch(
  find_package_root(getwd()),
  error = function(e) {
    if (!nzchar(script_path)) stop(e$message)
    find_package_root(dirname(script_path))
  }
)

extdata_dir <- file.path(repo_root, "inst", "extdata")
ipcc_dest <- file.path(extdata_dir, "ipcc_glossary.csv")
cache_update_script <- file.path(repo_root, "inst", "scripts", "update_bundled_caches.R")

if (!dir.exists(extdata_dir)) stop("Missing extdata directory: ", extdata_dir)
if (!file.exists(cache_update_script)) stop("Missing script: ", cache_update_script)

cat("Package root:", repo_root, "\n")
cat("Scraping IPCC glossary from live endpoint...\n")

source(file.path(repo_root, "R", "app.R"))
source(file.path(repo_root, "R", "data_ipcc.R"))

tmp_dir <- tempfile("ipcc_scrape_")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

scraped_path <- scrape_ipcc(cache_dir = tmp_dir)
if (!file.exists(scraped_path)) {
  stop("Scrape finished but output file not found: ", scraped_path)
}

ok <- file.copy(scraped_path, ipcc_dest, overwrite = TRUE)
if (!isTRUE(ok)) {
  stop("Failed to copy scraped file to: ", ipcc_dest)
}
cat("Updated:", ipcc_dest, "\n")

cat("Rebuilding bundled caches...\n")
rscript <- file.path(R.home("bin"), "Rscript")
cmd_out <- system2(
  command = rscript,
  args = c(cache_update_script, "--force"),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(cmd_out, "status") %||% 0L

if (length(cmd_out) > 0) cat(paste(cmd_out, collapse = "\n"), "\n")
if (!identical(as.integer(status), 0L)) {
  stop("Cache update script failed with status: ", status)
}

cat("Done.\n")
