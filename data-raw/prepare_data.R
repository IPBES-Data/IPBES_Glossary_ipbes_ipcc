# data-raw/prepare_data.R
# =============================================================================
# One-time developer script that populates inst/extdata/ with:
#   1. inst/extdata/ipbes_glossary.csv  — cleaned IPBES source
#   2. inst/extdata/ipcc_glossary.csv   — scraped IPCC snapshot
#   3. inst/extdata/ipcc_glossary_multilingual.csv — multilingual IPCC matrix
#   4. inst/extdata/glossary_multilingual_parquet/ — unified multilingual parquet
#   5. inst/extdata/glossary_multilingual_runtime.rds — runtime multilingual cache
#   6. inst/extdata/glossary_multilingual_unified.csv — inspection/back-compat export
#
# Run this script from the package root before building:
#   source("data-raw/prepare_data.R")
#
# Commit inst/extdata/ to git after running.
# =============================================================================

# -- Paths -------------------------------------------------------------------
# Script assumes it is sourced from the package root, or that getwd() is the
# package root (i.e. the directory containing DESCRIPTION).
repo_root <- getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the package root directory ",
       "(the one containing DESCRIPTION).")
}
cat("Package root:", repo_root, "\n\n")

ipbes_src   <- file.path(repo_root, "data-raw", "IPBES", "glossary_2026-02-23.csv")
ipbes_dest  <- file.path(repo_root, "inst", "extdata", "ipbes_glossary.csv")
ipcc_dest   <- file.path(repo_root, "inst", "extdata", "ipcc_glossary.csv")
ipcc_multi_dest <- file.path(repo_root, "inst", "extdata", "ipcc_glossary_multilingual.csv")
unified_multi_parquet_dir <- file.path(repo_root, "inst", "extdata", "glossary_multilingual_parquet")
unified_multi_runtime_rds <- file.path(repo_root, "inst", "extdata", "glossary_multilingual_runtime.rds")
unified_multi_csv <- file.path(repo_root, "inst", "extdata", "glossary_multilingual_unified.csv")
extdata_dir <- file.path(repo_root, "inst", "extdata")

if (!dir.exists(extdata_dir)) dir.create(extdata_dir, recursive = TRUE)

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

# Clean HTML entities from Definition column
def_col <- "Definition"
if (def_col %in% names(ipbes_raw)) {
  ipbes_raw[[def_col]] <- gsub("<[^>]+>", "",    ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&amp;",   "&",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&lt;",    "<",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&gt;",    ">",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&quot;",  "\"",  ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&#39;",   "'",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("&nbsp;",  " ",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- gsub("\\s+",    " ",   ipbes_raw[[def_col]])
  ipbes_raw[[def_col]] <- trimws(ipbes_raw[[def_col]])
}

write.csv(ipbes_raw, ipbes_dest, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Written to %s (%d rows).\n\n", ipbes_dest, nrow(ipbes_raw)))

# ============================================================
# Part 2: Scrape and cache IPCC glossary
# ============================================================
cat("=== Part 2: IPCC glossary (scrape from web) ===\n")
cat("This will take several minutes. Please be patient.\n\n")

# Load required packages
required_pkgs <- c("httr", "rvest", "dplyr", "arrow")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace,
                                       quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}

BASE_URL <- "https://apps.ipcc.ch/glossary"

PREFIXES <- c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
              "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
              "T", "U", "V", "W", "Y", "Z")

SESSION_HEADERS <- httr::add_headers(
  `User-Agent` = "glossary.ipbes.ipcc-data-prep/1.0 (https://github.com/rkrug/glossary_ipbes_ipcc)",
  `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
)

# -- Helper: clean term name (strip working group suffixes) ------------------
clean_term_name <- function(raw_text) {
  # Remove « WGI,WGII » style suffixes (unicode guillemets)
  cleaned <- gsub("\\s*\u00ab[^\u00bb]*\u00bb\\s*", "", raw_text)
  # Fallback: also strip ASCII angle brackets
  cleaned <- gsub("\\s*<<[^>]*>>\\s*", "", cleaned)
  trimws(cleaned)
}

# -- Step 1: collect term stubs ----------------------------------------------
cat("Step 1: Collecting term list by letter...\n")
term_stubs <- data.frame(id = character(), term = character(),
                         prefix = character(), stringsAsFactors = FALSE)

for (prefix in PREFIXES) {
  cat(sprintf("  Fetching letter '%s'... ", prefix))
  # search.php endpoint family (all reports)
  url  <- paste0(BASE_URL, "/ajax/ajax.searchbyindex.php?q=", prefix)
  html <- tryCatch({
    resp <- httr::GET(url, SESSION_HEADERS, httr::timeout(15))
    httr::stop_for_status(resp)
    httr::content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) {
    message("  Error: ", conditionMessage(e)); NULL
  })

  if (!is.null(html)) {
    page  <- rvest::read_html(html)
    # IPCC uses data-phraseid (not data-id) on span.alllink elements
    nodes <- rvest::html_elements(page, "span.alllink[data-phraseid]")
    if (length(nodes) > 0) {
      ids   <- rvest::html_attr(nodes, "data-phraseid")
      terms <- rvest::html_text(nodes, trim = TRUE)
      terms <- vapply(terms, clean_term_name, character(1), USE.NAMES = FALSE)
      valid <- !is.na(ids) & nchar(ids) > 0
      n_new <- sum(valid)
      cat(sprintf("%d terms\n", n_new))
      term_stubs <- rbind(term_stubs,
                          data.frame(id = ids[valid], term = terms[valid],
                                     prefix = prefix, stringsAsFactors = FALSE))
    } else {
      cat("0 terms (check HTML)\n")
    }
  } else {
    cat("FAILED\n")
  }
  Sys.sleep(0.3)
}

term_stubs <- term_stubs[!duplicated(term_stubs$id), ]
cat(sprintf("\nTotal unique terms: %d\n\n", nrow(term_stubs)))

if (nrow(term_stubs) == 0) {
  stop("No terms found. Site structure may have changed.")
}

# -- Step 2: fetch definitions -----------------------------------------------
# The detail endpoint is ajax.searchalloccurance.php (all reports)
cat("Step 2: Fetching definitions...\n")
all_rows <- vector("list", nrow(term_stubs))

for (i in seq_len(nrow(term_stubs))) {
  stub <- term_stubs[i, ]
  cat(sprintf("  [%d/%d] %s\n", i, nrow(term_stubs), stub$term))

  url  <- paste0(BASE_URL, "/ajax/ajax.searchalloccurance.php?q=",
                 stub$id, "&r=")
  html <- tryCatch({
    resp <- httr::GET(url, SESSION_HEADERS, httr::timeout(15))
    httr::stop_for_status(resp)
    httr::content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)

  definition <- NA_character_
  reports    <- NA_character_

  if (!is.null(html) && nchar(trimws(html)) > 10) {
    page <- rvest::read_html(html)

    # Definitions are in <p> tags within <dd> elements
    def_nodes <- rvest::html_elements(page, "dd p")
    if (length(def_nodes) > 0) {
      definition <- rvest::html_text(def_nodes[[1]], trim = TRUE)
    } else {
      # Fallback: try the whole <dd> text
      dd_nodes <- rvest::html_elements(page, "dd")
      if (length(dd_nodes) > 0) {
        definition <- rvest::html_text(dd_nodes[[1]], trim = TRUE)
      }
    }

    # Report labels from <h5> elements or data-report attributes
    rpt_nodes <- rvest::html_elements(page, "[data-report]")
    rpts      <- unique(rvest::html_attr(rpt_nodes, "data-report"))
    rpts      <- rpts[!is.na(rpts) & nchar(rpts) > 0]
    reports   <- paste(rpts, collapse = "; ")
  }

  all_rows[[i]] <- data.frame(
    id            = stub$id,
    prefix        = stub$prefix,
    term          = stub$term,
    definition    = definition,
    reports       = reports,
    downloaded_at = format(Sys.Date()),
    stringsAsFactors = FALSE
  )

  Sys.sleep(0.3)
}

# -- Step 3: save ------------------------------------------------------------
glossary_df <- do.call(rbind, all_rows)
write.csv(glossary_df, ipcc_dest, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\nSaved %d IPCC terms to %s\n", nrow(glossary_df), ipcc_dest))

# -- Step 4: scrape multilingual matrix --------------------------------------
cat("\nStep 4: Fetching multilingual IPCC matrix...\n")
source(file.path(repo_root, "R", "data_ipcc.R"))
scrape_ipcc_multilingual(cache_dir = extdata_dir)
if (!file.exists(ipcc_multi_dest)) {
  stop("Expected multilingual artifact not found: ", ipcc_multi_dest)
}
cat(sprintf("Saved multilingual IPCC matrix to %s\n", ipcc_multi_dest))

# -- Step 5: build unified multilingual artifacts -----------------------------
cat("\nStep 5: Building unified multilingual dataset (IPBES + IPCC)...\n")
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_multilingual.R"))

unified_multi <- build_unified_multilingual_dataset(
  ipbes_path = ipbes_dest,
  ipcc_multilingual_path = ipcc_multi_dest
)

write_unified_multilingual_artifacts(
  unified_df = unified_multi,
  parquet_dir = unified_multi_parquet_dir,
  runtime_rds_path = unified_multi_runtime_rds,
  unified_csv_path = unified_multi_csv
)

cat(sprintf("Saved unified parquet dataset to %s\n", unified_multi_parquet_dir))
cat(sprintf("Saved runtime multilingual cache to %s\n", unified_multi_runtime_rds))
cat(sprintf("Saved unified multilingual CSV export to %s\n", unified_multi_csv))

# ============================================================
# Part 3: Build bundled merged startup cache
# ============================================================
cat("\n=== Part 3: Build bundled merged startup cache ===\n")

# Source package functions needed for merge cache build
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_ipbes.R"))
source(file.path(repo_root, "R", "data_ipcc.R"))
source(file.path(repo_root, "R", "similarity_text.R"))
source(file.path(repo_root, "R", "data_merge.R"))
source(file.path(repo_root, "R", "mod_table.R"))
source(file.path(repo_root, "R", "hierarchy_terms.R"))
source(file.path(repo_root, "R", "mod_graph.R"))

ipbes_long <- load_ipbes(path = ipbes_dest)
ipbes_sum  <- summarise_ipbes(ipbes_long)
ipcc_raw   <- load_ipcc(cache_dir = tempdir(), bundled_path = ipcc_dest)
ipcc_sum   <- summarise_ipcc(ipcc_raw)
merged     <- merge_glossaries(ipbes_sum, ipcc_sum)
merged     <- .prepare_table_data(merged)

cache_meta <- list(
  schema    = 1L,
  ipbes_md5 = unname(as.character(tools::md5sum(ipbes_dest)[[1]])),
  ipcc_md5  = unname(as.character(tools::md5sum(ipcc_dest)[[1]]))
)

merged_cache_path <- file.path(extdata_dir, "merged_glossary_cache.rds")
saveRDS(list(meta = cache_meta, merged = merged), merged_cache_path)
cat(sprintf("Saved startup cache (%d rows) to %s\n",
            nrow(merged), merged_cache_path))

# Build bundled hierarchy edge cache so graph is available immediately.
hier_edges <- compute_term_hierarchy(
  merged_data = merged,
  min_score = 0,
  best_parent_only = FALSE
)
hier_cache <- list(
  meta = .hierarchy_cache_meta(merged),
  edges = hier_edges
)
hier_cache_path <- file.path(extdata_dir, "hierarchy_edges_cache.rds")
saveRDS(hier_cache, hier_cache_path)
cat(sprintf("Saved hierarchy cache (%d edges) to %s\n",
            nrow(hier_edges), hier_cache_path))

cat("\n=== Done. Commit inst/extdata/ to git. ===\n")
