#!/usr/bin/env Rscript

# Update bundled cache artifacts in inst/extdata based on the current
# ipbes_glossary.csv and ipcc_glossary.csv snapshots.
#
# Usage (from package root or any subdirectory):
#   Rscript inst/scripts/update_bundled_caches.R
#   Rscript inst/scripts/update_bundled_caches.R --force

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

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

script_path <- {
  full_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", full_args, value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)
  } else {
    ""
  }
}

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

repo_root <- tryCatch(
  find_package_root(getwd()),
  error = function(e) {
    if (!nzchar(script_path)) stop(e$message)
    find_package_root(dirname(script_path))
  }
)
extdata_dir <- file.path(repo_root, "inst", "extdata")
ipbes_path <- file.path(extdata_dir, "ipbes_glossary.csv")
ipcc_path <- file.path(extdata_dir, "ipcc_glossary.csv")
merged_cache_path <- file.path(extdata_dir, "merged_glossary_cache.rds")
hier_cache_path <- file.path(extdata_dir, "hierarchy_edges_cache.rds")

if (!dir.exists(extdata_dir)) stop("Missing extdata directory: ", extdata_dir)
if (!file.exists(ipbes_path)) stop("Missing file: ", ipbes_path)
if (!file.exists(ipcc_path)) stop("Missing file: ", ipcc_path)

cat("Package root:", repo_root, "\n")
cat("Force rebuild:", if (force) "yes" else "no", "\n")

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_ipbes.R"))
source(file.path(repo_root, "R", "data_ipcc.R"))
source(file.path(repo_root, "R", "similarity_text.R"))
source(file.path(repo_root, "R", "data_merge.R"))
source(file.path(repo_root, "R", "mod_table.R"))
source(file.path(repo_root, "R", "hierarchy_terms.R"))
source(file.path(repo_root, "R", "mod_graph.R"))

ipbes_md5 <- unname(as.character(tools::md5sum(ipbes_path)[[1]]))
ipcc_md5 <- unname(as.character(tools::md5sum(ipcc_path)[[1]]))

expected_merged_meta <- list(
  schema = 1L,
  ipbes_md5 = ipbes_md5,
  ipcc_md5 = ipcc_md5
)

existing_merged_meta <- NULL
if (file.exists(merged_cache_path)) {
  existing_obj <- tryCatch(readRDS(merged_cache_path), error = function(e) NULL)
  if (is.list(existing_obj) && !is.null(existing_obj$meta)) {
    existing_merged_meta <- existing_obj$meta
  }
}

rebuild <- force || !identical(existing_merged_meta, expected_merged_meta)
if (!rebuild) {
  cat("Bundled caches are already up to date. Nothing to do.\n")
  quit(save = "no", status = 0)
}

cat("Rebuilding bundled caches from extdata CSV snapshots...\n")

ipbes_long <- load_ipbes(path = ipbes_path)
ipbes_sum <- summarise_ipbes(ipbes_long)
ipcc_raw <- load_ipcc(cache_dir = tempdir(), bundled_path = ipcc_path)
ipcc_sum <- summarise_ipcc(ipcc_raw)

merged <- merge_glossaries(ipbes_sum, ipcc_sum)
merged <- .prepare_table_data(merged)

saveRDS(
  list(meta = expected_merged_meta, merged = merged),
  merged_cache_path
)
cat(sprintf("Saved %s (%d rows)\n", merged_cache_path, nrow(merged)))

hier_edges <- compute_term_hierarchy(
  merged_data = merged,
  min_score = 0,
  best_parent_only = FALSE
)
saveRDS(
  list(meta = .hierarchy_cache_meta(merged), edges = hier_edges),
  hier_cache_path
)
cat(sprintf("Saved %s (%d edges)\n", hier_cache_path, nrow(hier_edges)))

cat("Done.\n")
