# SHARED — keep in sync with compare/R/ and glossary/R/
# IPBES glossary data loading
# =============================================================================

#' Load and clean the IPBES glossary
#'
#' Reads the bundled IPBES CSV (one row per concept x assessment) and returns a
#' tidy long tibble ready for summarisation.
#'
#' @param path Path to the IPBES CSV file.  Defaults to the copy bundled in
#'   `inst/extdata/`.
#' @return A [dplyr::tibble()] with columns:
#'   \describe{
#'     \item{term_id}{Numeric IPBES term identifier.}
#'     \item{concept}{Glossary term name (lower-cased original).}
#'     \item{definition}{Cleaned definition text.}
#'     \item{assessment}{Individual assessment name (split from `Deliverable(s)`).}
#'     \item{acronym}{Acronym, if any.}
#'     \item{alt_labels}{Alternative labels, if any.}
#'   }
#' @export
load_ipbes <- function(
    path = file.path(getwd(), "inst", "extdata", "ipbes_glossary.csv")
) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("IPBES glossary CSV not found at: ", path)
  }

  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                         encoding = "UTF-8")

  # Canonical column name map
  col_map <- c(
    "Concept"                                                    = "concept",
    "Glossary term \u00bb Taxonomy term \u00bb Acronym (indexed field)" = "acronym",
    "Definition"                                                 = "definition",
    "Deliverable(s)"                                             = "deliverables",
    "Glossary term \u00bb Taxonomy term \u00bb Alternative labels (indexed field)" = "alt_labels",
    "term_id"                                                    = "term_id",
    "node_id"                                                    = "node_id"
  )

  for (old in names(col_map)) {
    if (old %in% names(raw)) {
      names(raw)[names(raw) == old] <- col_map[[old]]
    }
  }

  # Drop 'Title' column (redundant)
  raw <- raw[, setdiff(names(raw), "Title"), drop = FALSE]

  # Ensure required columns exist
  for (col in c("concept", "definition", "deliverables", "term_id")) {
    if (!col %in% names(raw)) raw[[col]] <- NA_character_
  }
  if (!"acronym"    %in% names(raw)) raw$acronym    <- NA_character_
  if (!"alt_labels" %in% names(raw)) raw$alt_labels <- NA_character_

  # Clean HTML entities from definition
  raw$definition <- clean_html(raw$definition)

  # Split 'deliverables' on ", " to get one assessment per row
  # Some rows already list multiple assessments in one cell
  long <- do.call(rbind, lapply(seq_len(nrow(raw)), function(i) {
    assessments <- trimws(strsplit(raw$deliverables[i], ",\\s*")[[1]])
    if (length(assessments) == 0 || all(!nzchar(assessments))) {
      assessments <- NA_character_
    }
    data.frame(
      term_id    = raw$term_id[i],
      concept    = raw$concept[i],
      definition = raw$definition[i],
      assessment = assessments,
      acronym    = raw$acronym[i],
      alt_labels = raw$alt_labels[i],
      stringsAsFactors = FALSE
    )
  }))

  tibble::as_tibble(long)
}

# =============================================================================

#' Summarise IPBES glossary to one row per concept
#'
#' Groups the long-format IPBES tibble by `concept` and collapses multiple
#' assessment-level definitions into a summary row.
#'
#' @param ipbes_long Output of [load_ipbes()].
#' @return A [dplyr::tibble()] with columns:
#'   \describe{
#'     \item{concept}{Term name.}
#'     \item{n_assessments}{Number of distinct assessments.}
#'     \item{summary_definition}{Shortest non-trivial definition across
#'       assessments (proxy for the most concise formulation).}
#'     \item{ipbes_data}{List-column; each element is a tibble of
#'       assessment x definition rows for use in detail view.}
#'   }
#' @export
summarise_ipbes <- function(ipbes_long) {
  concepts <- unique(ipbes_long$concept)

  rows <- lapply(concepts, function(cpt) {
    sub <- ipbes_long[ipbes_long$concept == cpt, , drop = FALSE]

    # Build the per-assessment tibble (for detail row)
    detail <- tibble::tibble(
      assessment = sub$assessment,
      definition = sub$definition
    )
    # Remove exact duplicates
    detail <- unique(detail)
    detail <- detail[!is.na(detail$definition) & nzchar(detail$definition), ]

    # Choose summary definition: shortest non-trivial (>= 20 chars)
    defs <- sub$definition[!is.na(sub$definition) & nchar(sub$definition) >= 20]
    if (length(defs) == 0) {
      defs <- sub$definition[!is.na(sub$definition)]
    }
    summary_def <- if (length(defs) > 0) defs[which.min(nchar(defs))] else NA_character_

    # Don't include "see X" redirects as summary
    is_redirect <- grepl("^see\\b", summary_def, ignore.case = TRUE)
    if (is_redirect && length(defs) > 1) {
      other_defs <- defs[!grepl("^see\\b", defs, ignore.case = TRUE)]
      if (length(other_defs) > 0) {
        summary_def <- other_defs[which.min(nchar(other_defs))]
      }
    }

    list(
      concept            = cpt,
      n_assessments      = length(unique(sub$assessment[!is.na(sub$assessment)])),
      summary_definition = summary_def,
      ipbes_data         = detail
    )
  })

  tibble::tibble(
    concept            = vapply(rows, `[[`, character(1), "concept"),
    n_assessments      = vapply(rows, `[[`, integer(1),   "n_assessments"),
    summary_definition = vapply(rows, `[[`, character(1), "summary_definition"),
    ipbes_data         = lapply(rows, `[[`, "ipbes_data")
  )
}
