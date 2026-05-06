# SHARED — keep in sync with compare/R/ and glossary/R/
# Directed term hierarchy scoring
# =============================================================================

#' Compute directed term hierarchy edges using a subsumption score
#'
#' Builds directed parent -> child edges where parent terms are broader and child
#' terms are more specific. The score combines lexical subsumption, definition
#' containment, and definition similarity.
#'
#' @param merged_data A merged glossary data frame/tibble as produced by
#'   [merge_glossaries()], containing at least `matched_term`, `ipbes_data`,
#'   and `ipcc_data`.
#' @param min_score Minimum directed subsumption score for an edge to be kept.
#' @param lexsub_gate Minimum lexical subsumption gate for candidate filtering.
#' @param defcontain_gate Minimum definition-containment gate for candidate
#'   filtering.
#' @param best_parent_only Logical; if `TRUE`, keeps only the highest-scoring
#'   parent per child.
#' @return A data frame with columns:
#'   `parent_term`, `child_term`, `score`, `lex_sub`, `def_contain`,
#'   `def_sim`, `parent_token_n`, `child_token_n`.
#' @keywords internal
compute_term_hierarchy <- function(
    merged_data,
    min_score = 0.65,
    lexsub_gate = 0.50,
    defcontain_gate = 0.40,
    best_parent_only = TRUE
) {
  features <- .build_term_features(merged_data)
  if (nrow(features) == 0) {
    return(.empty_hierarchy_edges())
  }

  token_index <- .build_token_index(features$term_tokens)
  candidate_edges <- .score_hierarchy_candidates(
    features = features,
    token_index = token_index,
    min_score = min_score,
    lexsub_gate = lexsub_gate,
    defcontain_gate = defcontain_gate
  )

  if (nrow(candidate_edges) == 0) {
    return(candidate_edges)
  }

  if (isTRUE(best_parent_only)) {
    candidate_edges <- .select_best_parent_per_child(candidate_edges)
  }

  candidate_edges[order(-candidate_edges$score, candidate_edges$child_term), , drop = FALSE]
}

# ---- Feature preparation ----------------------------------------------------

.build_term_features <- function(merged_data) {
  if (is.null(merged_data) || nrow(merged_data) == 0) {
    return(data.frame(
      term = character(),
      term_tokens = I(list()),
      term_token_n = integer(),
      def_tokens = I(list()),
      combined_definition = character(),
      stringsAsFactors = FALSE
    ))
  }

  terms <- as.character(merged_data$matched_term)
  terms <- terms[!is.na(terms)]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]
  terms <- unique(terms)
  if (length(terms) == 0) {
    return(data.frame(
      term = character(),
      term_tokens = I(list()),
      term_token_n = integer(),
      def_tokens = I(list()),
      combined_definition = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(terms, function(term) {
    idx <- which(merged_data$matched_term == term)[1]
    row <- merged_data[idx, , drop = FALSE]
    combined_def <- .collect_term_definition_text(row)
    def_tf <- tokenise_text(combined_def)
    def_tokens <- names(def_tf)
    if (is.null(def_tokens)) def_tokens <- character(0)
    term_tokens <- .tokenise_term_label(term)

    list(
      term = term,
      term_tokens = term_tokens,
      term_token_n = length(term_tokens),
      def_tokens = def_tokens,
      combined_definition = combined_def
    )
  })

  data.frame(
    term = vapply(rows, `[[`, character(1), "term"),
    term_tokens = I(lapply(rows, `[[`, "term_tokens")),
    term_token_n = vapply(rows, `[[`, integer(1), "term_token_n"),
    def_tokens = I(lapply(rows, `[[`, "def_tokens")),
    combined_definition = vapply(rows, `[[`, character(1), "combined_definition"),
    stringsAsFactors = FALSE
  )
}

.collect_term_definition_text <- function(row) {
  defs <- character(0)

  if ("ipbes_data" %in% names(row) && length(row$ipbes_data) > 0) {
    ipbes_df <- row$ipbes_data[[1]]
    if (!is.null(ipbes_df) && nrow(ipbes_df) > 0 && "definition" %in% names(ipbes_df)) {
      defs <- c(defs, as.character(ipbes_df$definition))
    }
  }

  if ("ipcc_data" %in% names(row) && length(row$ipcc_data) > 0) {
    ipcc_df <- row$ipcc_data[[1]]
    if (!is.null(ipcc_df) && nrow(ipcc_df) > 0 && "definition" %in% names(ipcc_df)) {
      defs <- c(defs, as.character(ipcc_df$definition))
    }
  }

  defs <- defs[!is.na(defs)]
  defs <- trimws(defs)
  defs <- defs[nzchar(defs)]
  defs <- clean_html(defs)
  defs <- unique(defs)
  if (length(defs) == 0) return("")
  paste(defs, collapse = " ")
}

.tokenise_term_label <- function(term) {
  if (is.null(term) || is.na(term) || !nzchar(trimws(term))) return(character(0))
  x <- tolower(term)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) return(character(0))
  toks <- unlist(strsplit(x, "\\s+"))
  toks <- toks[nzchar(toks)]
  unique(toks)
}

.build_token_index <- function(token_lists) {
  idx <- list()
  for (i in seq_along(token_lists)) {
    toks <- token_lists[[i]]
    if (length(toks) == 0) next
    for (tok in toks) {
      if (is.null(idx[[tok]])) {
        idx[[tok]] <- i
      } else {
        idx[[tok]] <- c(idx[[tok]], i)
      }
    }
  }
  idx
}

# ---- Edge scoring -----------------------------------------------------------

.score_hierarchy_candidates <- function(
    features,
    token_index,
    min_score,
    lexsub_gate,
    defcontain_gate
) {
  n <- nrow(features)
  out <- vector("list", length = 0L)

  for (child_id in seq_len(n)) {
    child_tokens <- features$term_tokens[[child_id]]
    child_token_n <- features$term_token_n[child_id]
    if (length(child_tokens) == 0 || child_token_n <= 1L) next

    candidate_ids <- unique(unlist(token_index[child_tokens], use.names = FALSE))
    if (length(candidate_ids) == 0) next
    candidate_ids <- candidate_ids[candidate_ids != child_id]
    candidate_ids <- candidate_ids[features$term_token_n[candidate_ids] < child_token_n]
    if (length(candidate_ids) == 0) next

    child_def_tokens <- features$def_tokens[[child_id]]

    for (parent_id in candidate_ids) {
      parent_tokens <- features$term_tokens[[parent_id]]
      parent_token_n <- features$term_token_n[parent_id]
      if (parent_token_n == 0L) next

      shared <- intersect(parent_tokens, child_tokens)
      if (length(shared) == 0) next

      lex_sub <- length(shared) / parent_token_n
      def_contain <- if (length(child_def_tokens) == 0) {
        0
      } else {
        length(intersect(parent_tokens, child_def_tokens)) / parent_token_n
      }

      if (lex_sub < lexsub_gate && def_contain < defcontain_gate) next

      def_sim <- compute_text_similarity(
        features$combined_definition[parent_id],
        features$combined_definition[child_id]
      )
      if (is.na(def_sim)) def_sim <- 0

      score <- 0.55 * lex_sub + 0.25 * def_contain + 0.20 * def_sim
      if (score < min_score) next

      out[[length(out) + 1L]] <- data.frame(
        parent_term = features$term[parent_id],
        child_term = features$term[child_id],
        score = score,
        lex_sub = lex_sub,
        def_contain = def_contain,
        def_sim = def_sim,
        parent_token_n = parent_token_n,
        child_token_n = child_token_n,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) return(.empty_hierarchy_edges())
  do.call(rbind, out)
}

.select_best_parent_per_child <- function(edges) {
  child_split <- split(edges, edges$child_term)
  best <- lapply(child_split, function(df) {
    df <- df[order(-df$score, -df$lex_sub, -df$def_contain, -df$def_sim, df$parent_term), , drop = FALSE]
    df[1, , drop = FALSE]
  })
  out <- do.call(rbind, best)
  row.names(out) <- NULL
  out
}

.empty_hierarchy_edges <- function() {
  data.frame(
    parent_term = character(),
    child_term = character(),
    score = numeric(),
    lex_sub = numeric(),
    def_contain = numeric(),
    def_sim = numeric(),
    parent_token_n = integer(),
    child_token_n = integer(),
    stringsAsFactors = FALSE
  )
}

