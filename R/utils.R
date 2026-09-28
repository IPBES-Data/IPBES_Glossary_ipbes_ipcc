# Utility functions and constants
# =============================================================================

#' Common English stopwords
#'
#' A character vector of ~60 common English stopwords used when tokenising
#' definition text for similarity computation.
#'

# =============================================================================

#' Strip HTML tags and decode common entities
#'
#' @param text Character vector.
#' @return Character vector with HTML removed.
#' @keywords internal
clean_html <- function(text) {
  if (is.null(text) || all(is.na(text))) return(text)
  # Non-breaking spaces are common in the source glossaries. Normalise them to
  # ordinary spaces: PCRE's \\s does not match U+00A0, so leaving them in place
  # breaks whole-word term matching and makes "range\u00a0of" unsearchable.
  text <- gsub("\u00a0", " ", text, useBytes = FALSE)
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

# =============================================================================

#' Require a UTF-8 capable locale
#'
#' The glossaries contain degree signs, en-dashes, curly quotes and
#' transliterated Sanskrit. In a non-UTF-8 locale (for example `C`, which is
#' what a bare `Rscript` often gets), R cannot represent those characters in the
#' native encoding and [utils::write.csv()] silently replaces each one with a
#' literal `<U+XXXX>` escape -- so `1.5°C pathway` is written out as
#' `1.5<U+00B0>C pathway` and stays that way in the app.
#'
#' This is called by the scraping and cache-building entry points, which write
#' the bundled snapshots. It attempts to switch to a UTF-8 locale and errors if
#' none is available, rather than letting a build corrupt the data.
#'
#' @param candidates Locale names to try, in order.
#' @return Invisibly, the active `LC_CTYPE` locale.
#' @keywords internal
.ensure_utf8_locale <- function(candidates = c("en_US.UTF-8", "C.UTF-8",
                                               "en_GB.UTF-8", "UTF-8")) {
  if (isTRUE(l10n_info()$`UTF-8`)) return(invisible(Sys.getlocale("LC_CTYPE")))

  for (loc in candidates) {
    ok <- tryCatch({
      suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
      isTRUE(l10n_info()$`UTF-8`)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(invisible(Sys.getlocale("LC_CTYPE")))
  }

  stop(
    "A UTF-8 locale is required to write the glossary snapshots without ",
    "corrupting non-ASCII characters, but none of these could be set: ",
    paste(candidates, collapse = ", "), ". Current LC_CTYPE is '",
    Sys.getlocale("LC_CTYPE"), "'. Re-run with, for example, ",
    "LC_ALL=en_US.UTF-8.",
    call. = FALSE
  )
}

#' Detect mangled non-ASCII escapes in character data
#'
#' Returns the indices of elements containing a literal `<U+XXXX>` sequence,
#' the signature of a UTF-8 string written out through a non-UTF-8 locale.
#'
#' @param x Character vector.
#' @return Integer vector of offending indices.
#' @keywords internal
.find_mangled_encoding <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  which(grepl("<U\\+[0-9A-Fa-f]{4,6}>", x))
}
