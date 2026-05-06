# SHARED — keep in sync with compare/R/ and glossary/R/
# Merge IPBES and IPCC glossaries
# =============================================================================

#' Merge IPBES and IPCC glossary summaries
#'
#' Performs a full outer join on normalised term names, returning one row per
#' matched or unmatched term.  Terms present in only one glossary are included
#' with `NA` values on the other side.
#'
#' The matching strategy is:
#' 1. Exact match on [normalise_term()]-processed strings.
#' 2. For unmatched IPBES concepts: retry after [strip_qualifier()].
#' 3. Any remaining unmatched rows are included as-is.
#'
#' @param ipbes_summary Output of [summarise_ipbes()].
#' @param ipcc_summary  Output of [summarise_ipcc()].
#' @return A [dplyr::tibble()] with columns:
#'   \describe{
#'     \item{matched_term}{Best display name for the term.}
#'     \item{ipbes_concept}{Original IPBES concept string, or `NA`.}
#'     \item{ipcc_term}{Original IPCC term string, or `NA`.}
#'     \item{ipbes_n_assessments}{Number of IPBES assessments, or `NA`.}
#'     \item{ipcc_n_reports}{Number of IPCC reports, or `NA`.}
#'     \item{ipbes_summary_def}{IPBES summary definition, or `NA`.}
#'     \item{ipcc_summary_def}{IPCC summary definition, or `NA`.}
#'     \item{ipbes_data}{List-column: per-assessment tibble from IPBES.}
#'     \item{ipcc_data}{List-column: per-report tibble from IPCC.}
#'     \item{sim_within_ipbes}{Mean pairwise similarity among IPBES definitions.}
#'     \item{sim_within_ipcc}{Mean pairwise similarity among IPCC definitions.}
#'     \item{sim_between_all}{Mean cross-similarity across all IPBES x IPCC definitions.}
#'   }
#' @export
merge_glossaries <- function(ipbes_summary, ipcc_summary) {

  # Normalise keys
  ipbes_summary$key <- normalise_term(ipbes_summary$concept)
  if (nrow(ipcc_summary) > 0) {
    ipcc_summary$key <- normalise_term(ipcc_summary$term)
  } else {
    ipcc_summary$key <- character(0)
  }

  # -- Pass 1: exact match ---------------------------------------------------
  matched_ipbes <- character(0)
  matched_ipcc  <- character(0)
  merged_rows   <- list()

  if (nrow(ipcc_summary) > 0) {
    ipcc_keys <- stats::setNames(seq_len(nrow(ipcc_summary)), ipcc_summary$key)

    for (i in seq_len(nrow(ipbes_summary))) {
      k <- ipbes_summary$key[i]
      j <- ipcc_keys[k]

      if (!is.na(j)) {
        merged_rows[[length(merged_rows) + 1]] <- .make_merged_row(
          ipbes_summary[i, ], ipcc_summary[j, ]
        )
        matched_ipbes <- c(matched_ipbes, k)
        matched_ipcc  <- c(matched_ipcc,  ipcc_summary$key[j])
        next
      }

      # -- Pass 2: retry with stripped qualifier ----------------------------
      k2 <- normalise_term(strip_qualifier(ipbes_summary$concept[i]))
      j2 <- ipcc_keys[k2]
      if (!is.na(j2) && !ipcc_summary$key[j2] %in% matched_ipcc) {
        merged_rows[[length(merged_rows) + 1]] <- .make_merged_row(
          ipbes_summary[i, ], ipcc_summary[j2, ]
        )
        matched_ipbes <- c(matched_ipbes, k)
        matched_ipcc  <- c(matched_ipcc,  ipcc_summary$key[j2])
        next
      }

      # -- IPBES-only row ---------------------------------------------------
      merged_rows[[length(merged_rows) + 1]] <- .make_merged_row(
        ipbes_summary[i, ], NULL
      )
      matched_ipbes <- c(matched_ipbes, k)
    }
  } else {
    # No IPCC data - all IPBES-only
    for (i in seq_len(nrow(ipbes_summary))) {
      merged_rows[[length(merged_rows) + 1]] <- .make_merged_row(
        ipbes_summary[i, ], NULL
      )
    }
  }

  # -- IPCC-only rows --------------------------------------------------------
  if (nrow(ipcc_summary) > 0) {
    unmatched_ipcc <- ipcc_summary[!ipcc_summary$key %in% matched_ipcc, ,
                                   drop = FALSE]
    for (i in seq_len(nrow(unmatched_ipcc))) {
      merged_rows[[length(merged_rows) + 1]] <- .make_merged_row(
        NULL, unmatched_ipcc[i, ]
      )
    }
  }

  # -- Assemble tibble -------------------------------------------------------
  out <- do.call(rbind, lapply(merged_rows, function(r) {
    data.frame(
      matched_term       = r$matched_term,
      ipbes_concept      = r$ipbes_concept,
      ipcc_term          = r$ipcc_term,
      ipbes_n_assessments = r$ipbes_n_assessments,
      ipcc_n_reports     = r$ipcc_n_reports,
      ipbes_summary_def  = r$ipbes_summary_def,
      ipcc_summary_def   = r$ipcc_summary_def,
      sim_within_ipbes   = NA_real_,
      sim_within_ipcc    = NA_real_,
      sim_between_all    = NA_real_,
      stringsAsFactors   = FALSE
    )
  }))

  # Attach list-columns
  out$ipbes_data <- lapply(merged_rows, `[[`, "ipbes_data")
  out$ipcc_data  <- lapply(merged_rows, `[[`, "ipcc_data")

  # Compute similarity metrics from all available definitions
  metrics <- lapply(seq_len(nrow(out)), function(i) {
    compute_similarity_triplet(out$ipbes_data[[i]], out$ipcc_data[[i]])
  })
  out$sim_within_ipbes <- vapply(metrics, `[[`, numeric(1), "within_ipbes")
  out$sim_within_ipcc  <- vapply(metrics, `[[`, numeric(1), "within_ipcc")
  out$sim_between_all  <- vapply(metrics, `[[`, numeric(1), "between_all")

  # Sort alphabetically by default
  out <- out[order(out$matched_term, na.last = TRUE), ]
  row.names(out) <- NULL

  tibble::as_tibble(out)
}

# Helper: construct a single merged row from (possibly NULL) summaries
.make_merged_row <- function(ipbes_row, ipcc_row) {
  # matched_term: prefer IPBES concept (more descriptive), fall back to IPCC
  if (!is.null(ipbes_row)) {
    matched_term      <- ipbes_row$concept
    ipbes_concept     <- ipbes_row$concept
    ipbes_n_assess    <- ipbes_row$n_assessments
    ipbes_summary_def <- ipbes_row$summary_definition
    ipbes_data        <- ipbes_row$ipbes_data[[1]]
  } else {
    matched_term      <- ipcc_row$term
    ipbes_concept     <- NA_character_
    ipbes_n_assess    <- NA_integer_
    ipbes_summary_def <- NA_character_
    ipbes_data        <- tibble::tibble(assessment = character(),
                                        definition = character())
  }

  if (!is.null(ipcc_row)) {
    ipcc_term         <- ipcc_row$term
    ipcc_n_reports    <- ipcc_row$n_reports
    ipcc_summary_def  <- ipcc_row$summary_definition
    ipcc_data         <- ipcc_row$ipcc_data[[1]]
  } else {
    ipcc_term         <- NA_character_
    ipcc_n_reports    <- NA_integer_
    ipcc_summary_def  <- NA_character_
    ipcc_data         <- tibble::tibble(report     = character(),
                                        definition = character())
  }

  list(
    matched_term       = matched_term,
    ipbes_concept      = ipbes_concept,
    ipcc_term          = ipcc_term,
    ipbes_n_assessments = ipbes_n_assess,
    ipcc_n_reports     = ipcc_n_reports,
    ipbes_summary_def  = ipbes_summary_def,
    ipcc_summary_def   = ipcc_summary_def,
    ipbes_data         = ipbes_data,
    ipcc_data          = ipcc_data
  )
}
