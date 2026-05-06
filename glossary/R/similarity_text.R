# SHARED — keep in sync with compare/R/ and glossary/R/
# Text-based similarity computation (pure base R)
# =============================================================================

#' Tokenise text into a term-frequency vector
#'
#' Lowercases, removes punctuation, splits on whitespace, and removes common
#' English stopwords (see [STOPWORDS]). No stemming or lemmatization is applied,
#' so inflections like "impact" and "impacts" are treated as different tokens.
#'
#' @param text A single character string.
#' @return A named integer vector of term frequencies, or an empty vector if
#'   `text` is `NA` or empty.
#' @keywords internal
tokenise_text <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(integer(0))
  text  <- tolower(text)
  text  <- gsub("[^a-z ]", " ", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[nzchar(words)]
  words <- words[!words %in% STOPWORDS]
  if (length(words) == 0) return(integer(0))
  table(words)
}

# =============================================================================

#' Compute cosine similarity between two text strings
#'
#' Uses TF (term frequency) vectors with cosine distance.  No IDF weighting is
#' applied; both texts are assumed to be short definition sentences.
#'
#' @param text_a,text_b Character strings to compare.
#' @return Numeric scalar in \[0, 1\], or `NA` if either input is empty/`NA`.
#' @export
compute_text_similarity <- function(text_a, text_b) {
  a <- tokenise_text(text_a)
  b <- tokenise_text(text_b)

  if (length(a) == 0 || length(b) == 0) return(NA_real_)

  shared <- intersect(names(a), names(b))
  if (length(shared) == 0) return(0)

  dot  <- sum(as.numeric(a[shared]) * as.numeric(b[shared]))
  norm_a <- sqrt(sum(as.numeric(a)^2))
  norm_b <- sqrt(sum(as.numeric(b)^2))

  if (norm_a == 0 || norm_b == 0) return(NA_real_)
  as.numeric(dot / (norm_a * norm_b))
}

# =============================================================================

#' Compute mean pairwise similarity within a set of definitions
#'
#' @param definitions Character vector of definition texts.
#' @return Mean cosine similarity across all unique pairs, or `NA` if fewer than
#'   two non-empty definitions are available.
#' @keywords internal
mean_within_similarity <- function(definitions) {
  defs <- .clean_definition_vector(definitions)
  if (length(defs) < 2) return(NA_real_)

  pairs <- utils::combn(seq_along(defs), 2)
  sims <- vapply(seq_len(ncol(pairs)), function(i) {
    compute_text_similarity(defs[pairs[1, i]], defs[pairs[2, i]])
  }, numeric(1))
  sims <- sims[!is.na(sims)]

  if (length(sims) == 0) NA_real_ else mean(sims)
}

#' Compute mean cross similarity between two sets of definitions
#'
#' @param definitions_a Character vector of definition texts (set A).
#' @param definitions_b Character vector of definition texts (set B).
#' @return Mean cosine similarity across all A x B pairs, or `NA` if either set
#'   is empty after cleaning.
#' @keywords internal
mean_between_similarity <- function(definitions_a, definitions_b) {
  defs_a <- .clean_definition_vector(definitions_a)
  defs_b <- .clean_definition_vector(definitions_b)
  if (length(defs_a) == 0 || length(defs_b) == 0) return(NA_real_)

  sims <- numeric(0)
  for (a in defs_a) {
    for (b in defs_b) {
      s <- compute_text_similarity(a, b)
      if (!is.na(s)) sims <- c(sims, s)
    }
  }

  if (length(sims) == 0) NA_real_ else mean(sims)
}

#' Compute the three similarity metrics for a merged glossary row
#'
#' @param ipbes_data IPBES detail tibble with a `definition` column.
#' @param ipcc_data  IPCC detail tibble with a `definition` column.
#' @return Named list with `within_ipbes`, `within_ipcc`, and `between_all`.
#' @keywords internal
compute_similarity_triplet <- function(ipbes_data, ipcc_data) {
  ipbes_defs <- if (!is.null(ipbes_data) &&
                    nrow(ipbes_data) > 0 &&
                    "definition" %in% names(ipbes_data)) {
    ipbes_data$definition
  } else {
    character(0)
  }

  ipcc_defs <- if (!is.null(ipcc_data) &&
                   nrow(ipcc_data) > 0 &&
                   "definition" %in% names(ipcc_data)) {
    ipcc_data$definition
  } else {
    character(0)
  }

  list(
    within_ipbes = mean_within_similarity(ipbes_defs),
    within_ipcc  = mean_within_similarity(ipcc_defs),
    between_all  = mean_between_similarity(ipbes_defs, ipcc_defs)
  )
}

# Internal: clean and trim definitions (keep duplicates to reflect all reports)
.clean_definition_vector <- function(definitions) {
  if (length(definitions) == 0) return(character(0))
  defs <- as.character(definitions)
  defs <- defs[!is.na(defs)]
  defs <- trimws(defs)
  defs <- defs[nzchar(defs)]
  defs
}
