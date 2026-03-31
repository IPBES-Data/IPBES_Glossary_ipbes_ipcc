# Unified multilingual glossary dataset helpers
# =============================================================================

.split_ipbes_assessments <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x)) return("UNSPECIFIED")
  parts <- trimws(strsplit(x, ",\\s*")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) "UNSPECIFIED" else parts
}

.coerce_bool <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("1", "true", "yes", "y")
}

.normalize_id_token <- function(x, fallback = "") {
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x)) return(fallback)
  x
}

build_unified_multilingual_dataset <- function(ipbes_path, ipcc_multilingual_path) {
  if (!file.exists(ipbes_path)) {
    stop("IPBES source not found: ", ipbes_path)
  }
  if (!file.exists(ipcc_multilingual_path)) {
    stop("IPCC multilingual source not found: ", ipcc_multilingual_path)
  }

  ipbes_raw <- utils::read.csv(
    ipbes_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )

  # Canonicalize IPBES columns used by current package loaders.
  col_map <- c(
    "Concept" = "concept",
    "Definition" = "definition",
    "Deliverable(s)" = "deliverables",
    "Glossary term \u00bb Taxonomy term \u00bb Alternative labels (indexed field)" = "alt_labels",
    "term_id" = "term_id",
    "node_id" = "node_id"
  )
  for (old in names(col_map)) {
    if (old %in% names(ipbes_raw)) names(ipbes_raw)[names(ipbes_raw) == old] <- col_map[[old]]
  }

  for (col in c("concept", "definition", "deliverables", "term_id", "node_id", "alt_labels")) {
    if (!col %in% names(ipbes_raw)) ipbes_raw[[col]] <- NA_character_
  }

  ipbes_rows <- vector("list", nrow(ipbes_raw))
  for (i in seq_len(nrow(ipbes_raw))) {
    row <- ipbes_raw[i, , drop = FALSE]
    assessments <- .split_ipbes_assessments(row$deliverables[[1]])

    term_id <- .normalize_id_token(row$term_id[[1]])
    node_id <- .normalize_id_token(row$node_id[[1]])

    concept_id <- if (nzchar(term_id) && nzchar(node_id)) {
      paste0("ipbes:", term_id, ":", node_id)
    } else if (nzchar(term_id)) {
      paste0("ipbes:term:", term_id)
    } else if (nzchar(node_id)) {
      paste0("ipbes:node:", node_id)
    } else {
      paste0("ipbes:concept:", normalise_term(row$concept[[1]]))
    }

    ipbes_rows[[i]] <- data.frame(
      source = "IPBES",
      concept_id = concept_id,
      report_or_assessment = assessments,
      language = "en",
      term = trimws(as.character(row$concept[[1]])),
      definition = clean_html(row$definition[[1]]),
      alt_labels = trimws(as.character(row$alt_labels[[1]])),
      is_available = TRUE,
      is_fetched = TRUE,
      fetch_status = "source_csv",
      source_endpoint = "data-raw/IPBES/glossary_2026-02-23.csv",
      downloaded_at = format(Sys.Date()),
      stringsAsFactors = FALSE
    )
  }

  ipbes_df <- do.call(rbind, ipbes_rows)

  ipcc_raw <- utils::read.csv(
    ipcc_multilingual_path,
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )

  expected_ipcc <- c(
    "id", "report", "language", "term", "definition", "did", "pid",
    "is_translation_available", "is_translation_fetched", "fetch_status",
    "source_endpoint", "downloaded_at"
  )
  for (col in expected_ipcc) {
    if (!col %in% names(ipcc_raw)) ipcc_raw[[col]] <- NA_character_
  }

  ipcc_df <- data.frame(
    source = "IPCC",
    concept_id = paste0("ipcc:", trimws(as.character(ipcc_raw$id))),
    report_or_assessment = trimws(as.character(ipcc_raw$report)),
    language = trimws(as.character(ipcc_raw$language)),
    term = trimws(as.character(ipcc_raw$term)),
    definition = clean_html(ipcc_raw$definition),
    alt_labels = NA_character_,
    is_available = .coerce_bool(ipcc_raw$is_translation_available),
    is_fetched = .coerce_bool(ipcc_raw$is_translation_fetched),
    fetch_status = trimws(as.character(ipcc_raw$fetch_status)),
    source_endpoint = trimws(as.character(ipcc_raw$source_endpoint)),
    downloaded_at = trimws(as.character(ipcc_raw$downloaded_at)),
    stringsAsFactors = FALSE
  )

  # Normalize partition field and fill blanks.
  ipcc_df$report_or_assessment[is.na(ipcc_df$report_or_assessment) | !nzchar(ipcc_df$report_or_assessment)] <- "UNSPECIFIED"
  ipbes_df$report_or_assessment[is.na(ipbes_df$report_or_assessment) | !nzchar(ipbes_df$report_or_assessment)] <- "UNSPECIFIED"

  # Remove empty terms/definitions placeholders to NA, keep rows.
  ipbes_df$term[!nzchar(ipbes_df$term)] <- NA_character_
  ipcc_df$term[!nzchar(ipcc_df$term)] <- NA_character_
  ipbes_df$definition[!nzchar(ipbes_df$definition)] <- NA_character_
  ipcc_df$definition[!nzchar(ipcc_df$definition)] <- NA_character_

  out <- rbind(ipbes_df, ipcc_df)

  # Deduplicate key collisions with deterministic preference.
  key <- paste(out$source, out$concept_id, out$report_or_assessment, out$language, sep = "::")
  ord <- order(
    key,
    -as.integer(out$is_fetched),
    -nchar(ifelse(is.na(out$definition), "", out$definition)),
    na.last = TRUE
  )
  out <- out[ord, , drop = FALSE]
  key2 <- paste(out$source, out$concept_id, out$report_or_assessment, out$language, sep = "::")
  out <- out[!duplicated(key2), , drop = FALSE]

  tibble::as_tibble(out)
}

write_unified_multilingual_artifacts <- function(
    unified_df,
    parquet_dir,
    runtime_rds_path,
    unified_csv_path = NULL
) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write multilingual parquet dataset.")
  }

  if (dir.exists(parquet_dir)) unlink(parquet_dir, recursive = TRUE, force = TRUE)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  arrow::write_dataset(
    unified_df,
    path = parquet_dir,
    format = "parquet",
    partitioning = c("source", "language", "report_or_assessment"),
    existing_data_behavior = "overwrite"
  )

  runtime_obj <- list(
    meta = list(
      schema = 1L,
      generated_at = as.character(Sys.time()),
      n_rows = nrow(unified_df)
    ),
    data = unified_df
  )
  saveRDS(runtime_obj, runtime_rds_path)

  if (!is.null(unified_csv_path) && nzchar(unified_csv_path)) {
    utils::write.csv(unified_df, unified_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  invisible(list(
    parquet_dir = parquet_dir,
    runtime_rds_path = runtime_rds_path,
    unified_csv_path = unified_csv_path
  ))
}

load_unified_multilingual_runtime <- function(
    path = {
      p <- system.file("extdata", "glossary_multilingual_runtime.rds",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "glossary_multilingual_runtime.rds")
    }
) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  obj <- readRDS(path)
  if (!is.list(obj) || is.null(obj$data)) return(NULL)
  obj$data
}
