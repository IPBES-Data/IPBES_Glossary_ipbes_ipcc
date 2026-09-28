# data-raw/prepare_data.R
# =============================================================================
# One-time developer script that populates inst/extdata/ with:
#   1. inst/extdata/ipbes_glossary.csv  — cleaned IPBES source
#   2. inst/extdata/ipcc_glossary.csv   — scraped IPCC snapshot
#   3. inst/extdata/merged_glossary_cache.rds — merged + highlight cache
#
# Run this script from the package root:
#   source("data-raw/prepare_data.R")
#
# Commit inst/extdata/ to git after running.
# =============================================================================

repo_root <- getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the package root directory ",
       "(the one containing DESCRIPTION).")
}
cat("Package root:", repo_root, "\n\n")

ipbes_src   <- file.path(repo_root, "data-raw", "IPBES", "glossary_2026-02-23.csv")
ipbes_dest  <- file.path(repo_root, "inst", "extdata", "ipbes_glossary.csv")
ipcc_dest   <- file.path(repo_root, "inst", "extdata", "ipcc_glossary.csv")
extdata_dir <- file.path(repo_root, "inst", "extdata")

if (!dir.exists(extdata_dir)) dir.create(extdata_dir, recursive = TRUE)

# Package functions are the single implementation of loading, scraping and
# merging; this script only orchestrates them.
for (f in c("app.R", "utils.R", "data_ipbes.R", "data_ipcc.R",
            "data_merge.R", "ipcc_report_names.R", "app_glossary.R")) {
  source(file.path(repo_root, "R", f))

# Fail loudly rather than writing mangled non-ASCII into the snapshots.
.ensure_utf8_locale()
}

# ============================================================
# Part 1: Clean and copy IPBES glossary
# ============================================================
cat("=== Part 1: IPBES glossary ===\n")

if (!file.exists(ipbes_src)) {
  stop("IPBES source not found: ", ipbes_src)
}

ipbes_raw <- read.csv(ipbes_src, stringsAsFactors = FALSE,
                      check.names = FALSE, encoding = "UTF-8")
cat(sprintf("Read %d rows from IPBES source.\n", nrow(ipbes_raw)))

def_col <- "Definition"
if (def_col %in% names(ipbes_raw)) {
  ipbes_raw[[def_col]] <- clean_html(ipbes_raw[[def_col]])
}

write.csv(ipbes_raw, ipbes_dest, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Written to %s (%d rows).\n\n", ipbes_dest, nrow(ipbes_raw)))

# ============================================================
# Part 2: Scrape IPCC glossary
# ============================================================
cat("=== Part 2: IPCC glossary (scrape from web) ===\n")
cat("This will take several minutes. Please be patient.\n\n")

required_pkgs <- c("httr", "rvest")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}

tmp_dir <- tempfile("ipcc_scrape_")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

scraped_path <- scrape_ipcc(
  cache_dir = tmp_dir,
  progress_callback = function(current, total, term) {
    if (current %% 50 == 0 || current == total) {
      cat(sprintf("  [%d/%d] %s\n", current, total, term))
    }
  }
)

if (!file.copy(scraped_path, ipcc_dest, overwrite = TRUE)) {
  stop("Failed to copy scraped file to: ", ipcc_dest)
}
scraped <- read.csv(ipcc_dest, stringsAsFactors = FALSE)
cat(sprintf("\nSaved %d (term, report) rows covering %d terms to %s\n",
            nrow(scraped), length(unique(scraped$term)), ipcc_dest))

# ============================================================
# Part 3: Build bundled merged + highlight cache
# ============================================================
cat("\n=== Part 3: Build bundled merged startup cache ===\n")

ipbes_long <- load_ipbes(path = ipbes_dest)
ipbes_sum  <- summarise_ipbes(ipbes_long)
ipcc_raw   <- load_ipcc(cache_dir = tempdir(), bundled_path = ipcc_dest)
ipcc_sum   <- summarise_ipcc(ipcc_raw)
merged     <- merge_glossaries(ipbes_sum, ipcc_sum)
merged     <- .prepare_glossary_highlight_data(merged)

cache_meta <- list(
  schema    = 1L,
  ipbes_md5 = unname(as.character(tools::md5sum(ipbes_dest)[[1]])),
  ipcc_md5  = unname(as.character(tools::md5sum(ipcc_dest)[[1]]))
)

merged_cache_path <- file.path(extdata_dir, "merged_glossary_cache.rds")
saveRDS(list(meta = cache_meta, merged = merged), merged_cache_path)
cat(sprintf("Saved startup cache (%d rows, with highlight cache) to %s\n",
            nrow(merged), merged_cache_path))

cat("\n=== Done. Commit inst/extdata/ to git. ===\n")
