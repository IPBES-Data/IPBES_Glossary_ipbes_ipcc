source(file.path("R", "utils.R"))
source(file.path("R", "data_multilingual.R"))

expect <- function(ok, msg) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", msg))
  cat(sprintf("OK: %s\n", msg))
}

ipbes_path <- file.path("inst", "extdata", "ipbes_glossary.csv")
ipcc_multi_path <- file.path("inst", "extdata", "ipcc_glossary_multilingual.csv")
parquet_dir <- file.path("inst", "extdata", "glossary_multilingual_parquet")
runtime_rds <- file.path("inst", "extdata", "glossary_multilingual_runtime.rds")

u <- build_unified_multilingual_dataset(ipbes_path, ipcc_multi_path)

required <- c(
  "source", "concept_id", "report_or_assessment", "language", "term", "definition",
  "alt_labels", "is_available", "is_fetched", "fetch_status", "source_endpoint", "downloaded_at"
)
expect(all(required %in% names(u)), "required unified schema columns exist")

k <- paste(u$source, u$concept_id, u$report_or_assessment, u$language, sep = "::")
expect(length(unique(k)) == nrow(u), "key uniqueness source/concept/report_or_assessment/language")

ipbes <- u[u$source == "IPBES", , drop = FALSE]
expect(nrow(ipbes) > 0, "IPBES rows exist")
expect(all(ipbes$language == "en"), "IPBES rows are english only")

ipcc <- u[u$source == "IPCC", , drop = FALSE]
expect(nrow(ipcc) > 0, "IPCC rows exist")
expect(length(unique(ipcc$language)) >= 2, "IPCC contains multilingual rows")

expect(dir.exists(parquet_dir), "parquet dataset directory exists")
expect(file.exists(runtime_rds), "runtime RDS exists")

# Check partition layout contains source and language level partitions.
parts <- list.files(parquet_dir, recursive = TRUE, full.names = FALSE)
expect(any(grepl("^source=IPBES", parts)), "IPBES source partition exists")
expect(any(grepl("^source=IPCC", parts)), "IPCC source partition exists")
expect(any(grepl("language=en", parts, fixed = TRUE)), "language partition exists")

runtime <- readRDS(runtime_rds)
expect(is.list(runtime) && !is.null(runtime$data), "runtime RDS structure")
expect(nrow(runtime$data) == nrow(u), "runtime RDS row count matches unified builder")

if (requireNamespace("arrow", quietly = TRUE)) {
  ds <- arrow::open_dataset(parquet_dir, format = "parquet")
  n <- nrow(as.data.frame(ds))
  expect(n == nrow(u), "parquet row count matches unified builder")
}

cat("All unified multilingual dataset checks passed.\n")
