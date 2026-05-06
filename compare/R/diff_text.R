# SHARED — keep in sync with compare/R/ and glossary/R/
# Word-level text diff rendering
# =============================================================================

#' Render a word-level HTML diff between two texts
#'
#' Computes the longest common subsequence (LCS) of the word sequences and
#' renders the result as HTML with `<del>` spans for words only in `text_a` and
#' `<ins>` spans for words only in `text_b`.
#'
#' @param text_a,text_b Character strings to compare.
#' @param label_a,label_b Labels prepended to the output (default `"IPBES"` and
#'   `"IPCC"`).
#' @return An [htmltools::tag()] object.
#' @export
render_text_diff <- function(text_a, text_b, label_a = "IPBES", label_b = "IPCC") {
  if (is.na(text_a) || !nzchar(trimws(text_a))) text_a <- "(no definition)"
  if (is.na(text_b) || !nzchar(trimws(text_b))) text_b <- "(no definition)"
  .diff_lcs(text_a, text_b, label_a, label_b)
}

# ---- Built-in LCS word-level diff ----------------------------------------
.diff_lcs <- function(text_a, text_b, label_a, label_b) {
  words_a <- .split_words(text_a)
  words_b <- .split_words(text_b)

  ops <- .lcs_diff(words_a, words_b)

  parts_a <- lapply(ops[vapply(ops, function(o) o$type %in% c("same", "del"),
                               logical(1))], .op_to_tag)
  parts_b <- lapply(ops[vapply(ops, function(o) o$type %in% c("same", "ins"),
                               logical(1))], .op_to_tag)

  htmltools::div(
    class = "diff-text",
    htmltools::tags$strong(paste0(label_a, ":")),
    " ",
    .join_word_tags(parts_a),
    htmltools::tags$br(),
    htmltools::tags$strong(paste0(label_b, ":")),
    " ",
    .join_word_tags(parts_b)
  )
}

.op_to_tag <- function(op) {
  w <- op$word
  switch(op$type,
    same = htmltools::span(class = "diff-same", w),
    del  = htmltools::tags$del(class = "diff-del", w),
    ins  = htmltools::tags$ins(class = "diff-ins", w),
    htmltools::span(w)
  )
}

.join_word_tags <- function(parts) {
  if (length(parts) == 0) return(NULL)
  joined <- list(parts[[1]])
  if (length(parts) >= 2) {
    for (i in 2:length(parts)) {
      joined <- c(joined, list(" ", parts[[i]]))
    }
  }
  htmltools::tagList(joined)
}

.split_words <- function(text) {
  text  <- gsub("[[:punct:]]", " ", text)
  words <- unlist(strsplit(text, "\\s+"))
  words[nzchar(words)]
}

# Standard DP LCS -> list of (type, word) operations
.lcs_diff <- function(a, b) {
  m <- length(a); n <- length(b)

  # Build DP table
  dp <- matrix(0L, nrow = m + 1, ncol = n + 1)
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      if (a[i] == b[j]) {
        dp[i + 1, j + 1] <- dp[i, j] + 1L
      } else {
        dp[i + 1, j + 1] <- max(dp[i, j + 1], dp[i + 1, j])
      }
    }
  }

  # Backtrack
  ops <- list()
  i <- m; j <- n
  while (i > 0 || j > 0) {
    if (i > 0 && j > 0 && a[i] == b[j]) {
      ops[[length(ops) + 1]] <- list(type = "same", word = a[i])
      i <- i - 1; j <- j - 1
    } else if (j > 0 && (i == 0 || dp[i + 1, j] >= dp[i, j + 1])) {
      ops[[length(ops) + 1]] <- list(type = "ins", word = b[j])
      j <- j - 1
    } else {
      ops[[length(ops) + 1]] <- list(type = "del", word = a[i])
      i <- i - 1
    }
  }

  rev(ops)
}
