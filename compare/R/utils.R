# SHARED — keep in sync with compare/R/ and glossary/R/
# Utility functions and constants
# =============================================================================

#' Common English stopwords
#'
#' A character vector of ~60 common English stopwords used when tokenising
#' definition text for similarity computation.
#'
#' @keywords internal
STOPWORDS <- c(
  "a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for",
  "of", "with", "by", "from", "as", "is", "are", "was", "were", "be",
  "been", "being", "have", "has", "had", "do", "does", "did", "will",
  "would", "shall", "should", "may", "might", "can", "could", "that",
  "this", "these", "those", "it", "its", "not", "no", "nor", "so",
  "yet", "both", "either", "whether", "into", "through", "during",
  "between", "such", "more", "also", "which", "what", "when", "where",
  "how", "all", "any", "each", "their", "they", "them", "than", "other"
)

# =============================================================================

#' Strip HTML tags and decode common entities
#'
#' @param text Character vector.
#' @return Character vector with HTML removed.
#' @keywords internal
clean_html <- function(text) {
  if (is.null(text) || all(is.na(text))) return(text)
  text <- gsub("<[^>]+>", "", text)        # strip tags
  text <- gsub("&amp;",  "&",  text)
  text <- gsub("&lt;",   "<",  text)
  text <- gsub("&gt;",   ">",  text)
  text <- gsub("&quot;", "\"", text)
  text <- gsub("&#39;",  "'",  text)
  text <- gsub("&nbsp;", " ",  text)
  text <- gsub("\\s+",   " ",  text)       # collapse whitespace
  trimws(text)
}

# =============================================================================

#' Normalise a glossary term for matching
#'
#' Lowercases, strips non-alphanumeric characters (except spaces), and trims
#' whitespace.  Used to match IPBES concept names against IPCC term names.
#'
#' @param term Character vector of term names.
#' @return Normalised character vector.
#' @keywords internal
normalise_term <- function(term) {
  if (is.null(term) || all(is.na(term))) return(term)
  term <- tolower(term)
  term <- gsub("[^a-z0-9 ]", " ", term)
  term <- gsub("\\s+", " ", term)
  trimws(term)
}

# =============================================================================

#' Strip parenthetical qualifiers from a term
#'
#' Removes the first parenthetical expression, e.g.
#' `"abundance (ecological)"` becomes `"abundance"`.
#'
#' @param term Character vector.
#' @return Character vector with parenthetical qualifier removed.
#' @keywords internal
strip_qualifier <- function(term) {
  gsub("\\s*\\([^)]*\\)\\s*", " ", term) |> trimws()
}

# =============================================================================

#' Truncate text for table display
#'
#' @param text  Character vector.
#' @param n     Maximum number of characters (default 200).
#' @param ellipsis Suffix appended when truncated (default `"..."`).
#' @return Character vector.
#' @keywords internal
truncate_text <- function(text, n = 200, ellipsis = "\u2026") {
  ifelse(
    is.na(text) | nchar(text) <= n,
    text,
    paste0(substr(text, 1, n), ellipsis)
  )
}

# =============================================================================

#' Build a coloured similarity bar HTML snippet
#'
#' @param score Numeric 0-1 (or NA).
#' @return HTML string.
#' @keywords internal
similarity_bar_html <- function(score) {
  if (is.na(score)) {
    return(htmltools::span(
      style = "color: #aaa; font-size: 0.8rem;",
      "\u2014"   # em dash
    ) |> as.character())
  }
  pct <- round(score * 100, 1)
  # The bar is a coloured gradient; a grey overlay covers the "empty" portion
  right_pct <- 100 - pct
  htmltools::div(
    class = "sim-bar-wrap",
    htmltools::div(
      class = "sim-bar",
      htmltools::div(
        class = "sim-bar-fill",
        style = paste0("width:", right_pct, "%")
      )
    ),
    htmltools::span(class = "sim-label", paste0(pct, "%"))
  ) |> as.character()
}

# =============================================================================

#' Build a three-line similarity HTML snippet
#'
#' @param sim_within_ipbes Numeric 0-1 or `NA`.
#' @param sim_within_ipcc Numeric 0-1 or `NA`.
#' @param sim_between_all Numeric 0-1 or `NA`.
#' @return HTML string.
#' @keywords internal
similarity_triplet_html <- function(
    sim_within_ipbes,
    sim_within_ipcc,
    sim_between_all
) {
  rows <- c(
    .similarity_metric_row_html("Within IPBES", sim_within_ipbes),
    .similarity_metric_row_html("Within IPCC", sim_within_ipcc),
    .similarity_metric_row_html("Between All Definitions", sim_between_all)
  )

  htmltools::div(
    class = "sim-stack",
    htmltools::HTML(paste(rows, collapse = ""))
  ) |> as.character()
}

.similarity_metric_row_html <- function(label, score) {
  htmltools::div(
    class = "sim-metric-row",
    htmltools::span(class = "sim-metric-label", label),
    htmltools::HTML(similarity_bar_html(score))
  ) |> as.character()
}
