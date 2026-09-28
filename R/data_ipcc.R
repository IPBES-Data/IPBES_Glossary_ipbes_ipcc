# IPCC glossary data loading and scraping
# =============================================================================
# The IPCC glossary is a per-report structure: a term can appear in many
# reports, and each report may carry its own wording of the definition (or no
# definition at all, only a "See ..." cross-reference to another term).
# Everything below therefore works at (term, report) granularity.

# Resolve active IPCC source path (cache first, then bundled snapshot)
resolve_ipcc_source_path <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary.csv")
    }
) {
  cache_file <- file.path(cache_dir, "ipcc_glossary.csv")

  if (file.exists(cache_file) && file.info(cache_file)$size > 10) {
    return(cache_file)
  }

  if (nzchar(bundled_path) && file.exists(bundled_path) &&
      file.info(bundled_path)$size > 10) {
    return(bundled_path)
  }

  ""
}

#' Load the IPCC glossary
#'
#' Looks for a user-updated CSV in `cache_dir` first; falls back to the bundled
#' snapshot in `inst/extdata/`.
#'
#' The current snapshot format has one row per (term, report) pair. CSVs in the
#' older one-row-per-term format (with a semicolon-separated `reports` column)
#' are still readable: their report list is expanded, with the single stored
#' definition repeated across reports.
#'
#' @param cache_dir Directory where the user-updated IPCC CSV may be stored.
#'   Defaults to [tools::R_user_dir()] cache.
#' @param bundled_path Path to the bundled IPCC CSV.
#' @return A [tibble::tibble()] with columns `id`, `term`, `report`,
#'   `definition`, `xref_kind`, `xref_target`, `xref_target_id` and
#'   `downloaded_at`. Returns an empty tibble with the correct columns if no
#'   data is available.
#' @export
load_ipcc <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary.csv")
    }
) {
  path <- resolve_ipcc_source_path(cache_dir, bundled_path)
  if (!nzchar(path)) {
    return(.empty_ipcc_tibble())
  }

  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) {
      message("Could not read IPCC CSV: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) return(.empty_ipcc_tibble())

  # Legacy snapshots stored one row per term with a "reports" list column.
  if (!("report" %in% names(df)) && "reports" %in% names(df)) {
    df <- .ipcc_expand_legacy_reports(df)
  }

  for (col in .IPCC_COLUMNS) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df$definition <- clean_html(df$definition)
  df$report     <- trimws(as.character(df$report))
  tibble::as_tibble(df[, .IPCC_COLUMNS])
}

.IPCC_COLUMNS <- c("id", "term", "report", "definition",
                   "xref_kind", "xref_target", "xref_target_id",
                   "downloaded_at")

.empty_ipcc_tibble <- function() {
  out <- lapply(.IPCC_COLUMNS, function(x) character())
  names(out) <- .IPCC_COLUMNS
  tibble::as_tibble(out)
}

# Expand a legacy one-row-per-term frame into one row per (term, report).
.ipcc_expand_legacy_reports <- function(df) {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    rpts <- trimws(strsplit(as.character(df$reports[i]), ";\\s*")[[1]])
    rpts <- rpts[nzchar(rpts)]
    if (length(rpts) == 0) rpts <- NA_character_
    out <- df[rep(i, length(rpts)), setdiff(names(df), "reports"), drop = FALSE]
    out$report <- rpts
    out
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# =============================================================================

#' Summarise IPCC glossary to one row per term
#'
#' Definitions are kept exactly as the report words them. A report's related-term
#' pointers (`See`, `See Also`, `Sub-terms`) are carried in a separate `xref`
#' column so they can be displayed apart from the definition; a report entry may
#' have a definition, pointers, both, or neither.
#'
#' @param ipcc_df Output of [load_ipcc()].
#' @return A [tibble::tibble()] with columns `term`, `n_reports`,
#'   `summary_definition`, `ipcc_data` (list-column of
#'   report x definition x xref tibbles).
#' @export
summarise_ipcc <- function(ipcc_df) {
  if (nrow(ipcc_df) == 0) {
    return(tibble::tibble(
      term               = character(),
      n_reports          = integer(),
      summary_definition = character(),
      ipcc_data          = list()
    ))
  }

  ipcc_df$xref <- .ipcc_xref_display(ipcc_df)
  terms <- unique(ipcc_df$term)

  rows <- lapply(terms, function(trm) {
    sub <- ipcc_df[ipcc_df$term == trm, , drop = FALSE]

    detail <- unique(tibble::tibble(
      report     = sub$report,
      definition = ifelse(is.na(sub$definition), "", sub$definition),
      xref       = sub$xref
    ))

    defs <- detail$definition[!is.na(detail$definition) &
                                nchar(detail$definition) >= 20]
    if (length(defs) == 0) {
      defs <- detail$definition[!is.na(detail$definition) &
                                  nzchar(detail$definition)]
    }
    summary_def <- if (length(defs) > 0) defs[which.min(nchar(defs))] else NA_character_

    n_reports <- length(unique(detail$report[!is.na(detail$report) &
                                               nzchar(detail$report)]))

    list(
      term               = trm,
      n_reports          = n_reports,
      summary_definition = summary_def,
      ipcc_data          = detail
    )
  })

  tibble::tibble(
    term               = vapply(rows, `[[`, character(1), "term"),
    n_reports          = vapply(rows, `[[`, integer(1),   "n_reports"),
    summary_definition = vapply(rows, `[[`, character(1), "summary_definition"),
    ipcc_data          = lapply(rows, `[[`, "ipcc_data")
  )
}

# Render a row's related-term pointers as a display string.
#
# `xref_target` stores multiple targets joined with " | "; that is a storage
# delimiter and must not reach the UI. The label follows the relationship: a
# redirect reads "See:", a supplementary pointer "See also:", and a
# narrower-term list "Sub-terms:". Returns "" for rows with no pointers.
#
# This is deliberately kept separate from the definition text. IPCC pointers are
# report-specific editorial data and are displayed apart from the definition,
# not spliced into it.
.ipcc_xref_display <- function(df) {
  target <- if ("xref_target" %in% names(df)) {
    trimws(as.character(df$xref_target))
  } else {
    rep(NA_character_, nrow(df))
  }
  target[is.na(target)] <- ""

  kind <- if ("xref_kind" %in% names(df)) {
    as.character(df$xref_kind)
  } else {
    rep(NA_character_, nrow(df))
  }
  kind[is.na(kind)] <- ""

  out <- rep("", length(target))
  present <- nzchar(target)
  if (!any(present)) return(out)

  out[present] <- paste0(
    .ipcc_xref_label(kind[present]),
    gsub("\\s*\\|\\s*", ", ", target[present])
  )
  out
}

# Map a stored xref_kind to its display prefix. A redirect wins over a
# supplementary pointer when a row carries both.
.ipcc_xref_label <- function(kind) {
  ifelse(grepl("\\bsee\\b", kind), "See: ",
    ifelse(grepl("see_also", kind), "See also: ",
      ifelse(grepl("sub_terms", kind), "Sub-terms: ", "See: ")))
}

# =============================================================================
# Scraping helpers (shared by scrape_ipcc() and data-raw/prepare_data.R)
# =============================================================================

.IPCC_BASE_URL <- "https://apps.ipcc.ch/glossary"

.IPCC_PREFIXES <- c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
                    "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
                    "T", "U", "V", "W", "Y", "Z")

# Strip working-group suffixes such as "<< WGI,WGII >>" from a term label.
.ipcc_clean_term <- function(raw_text) {
  cleaned <- gsub("\\s*\u00ab[^\u00bb]*\u00bb\\s*", "", raw_text)
  cleaned <- gsub("\\s*<<[^>]*>>\\s*", "", cleaned)
  trimws(cleaned)
}

.ipcc_headers <- function() {
  httr::add_headers(
    `User-Agent` = paste0("glossary.ipbes.ipcc R package/",
                          .package_version_safe(),
                          " (https://github.com/rkrug/glossary_ipbes_ipcc)"),
    `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
  )
}

.ipcc_get <- function(url, timeout = 15) {
  tryCatch({
    resp <- httr::GET(url, .ipcc_headers(), httr::timeout(timeout))
    httr::stop_for_status(resp)
    httr::content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)
}

#' Parse the all-occurrences response into per-report rows
#'
#' The response carries one `<dd>` block per report. Each block holds the
#' report abbreviation (in a `data-report` attribute) and that report's own
#' wording of the definition, which frequently differs between reports and is
#' sometimes absent entirely.
#'
#' @param html Raw HTML text, or `NULL`.
#' @return A data frame with columns `report` and `definition`.
#' @keywords internal
.ipcc_parse_occurrences <- function(html) {
  empty <- data.frame(report = character(), definition = character(),
                      stringsAsFactors = FALSE)
  if (is.null(html) || !nzchar(trimws(html))) return(empty)

  page <- tryCatch(rvest::read_html(html), error = function(e) NULL)
  if (is.null(page)) return(empty)

  blocks <- rvest::html_elements(page, "dd")
  if (length(blocks) == 0) return(empty)

  rows <- lapply(blocks, function(block) {
    tagged <- rvest::html_elements(block, "[data-report]")
    report <- if (length(tagged) > 0) {
      rvest::html_attr(tagged[[1]], "data-report")
    } else {
      NA_character_
    }
    paragraphs <- rvest::html_elements(block, "p")
    definition <- if (length(paragraphs) > 0) {
      rvest::html_text(paragraphs[[1]], trim = TRUE)
    } else {
      ""
    }
    data.frame(report = report, definition = definition,
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  out$report     <- trimws(as.character(out$report))
  out$definition <- trimws(as.character(out$definition))
  out <- out[!is.na(out$report) & nzchar(out$report), , drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Parse the per-report response for related-term pointers
#'
#' The `ul.items` list is used for three different relationships, told apart by
#' the `<h6>` heading above it and the prefix on each `<li>`:
#'
#' * `see` -- a redirect, on an entry with no definition of its own
#'   ("See Detection and attribution")
#' * `see_also` -- a supplementary pointer, which may sit on an entry that does
#'   have a definition
#' * `sub_terms` -- under a "Sub-terms" heading, the narrower terms beneath this
#'   one; these carry no "See" prefix at all
#'
#' None of this is present in the all-occurrences response; it is only served
#' per report.
#'
#' @param html Raw HTML text, or `NULL`.
#' @return A one-row data frame with `xref_kind`, `xref_target` and
#'   `xref_target_id`. Multiple targets are joined with `" | "`, which is a
#'   storage delimiter only; `.ipcc_xref_display()` builds the display string.
#'   All fields are `NA` when the response carries no pointers.
#' @keywords internal
.ipcc_parse_cross_reference <- function(html) {
  none <- data.frame(xref_kind = NA_character_, xref_target = NA_character_,
                     xref_target_id = NA_character_, stringsAsFactors = FALSE)
  if (is.null(html) || !nzchar(trimws(html))) return(none)

  page <- tryCatch(rvest::read_html(html), error = function(e) NULL)
  if (is.null(page)) return(none)

  lists <- rvest::html_elements(page, "ul.items")
  if (length(lists) == 0) return(none)

  # The heading sits in the same container as the list.
  headings <- rvest::html_text(rvest::html_elements(page, "h6"), trim = TRUE)
  heading_kind <- if (any(grepl("sub-?term", headings, ignore.case = TRUE)) &&
                      !any(grepl("^\\s*see", headings, ignore.case = TRUE))) {
    "sub_terms"
  } else {
    NA_character_
  }

  items <- rvest::html_elements(page, "ul.items li")
  parsed <- lapply(items, function(item) {
    label <- rvest::html_text(item, trim = TRUE)
    kind <- if (grepl("^\\s*See\\s+Also", label, ignore.case = TRUE)) {
      "see_also"
    } else if (grepl("^\\s*See\\b", label, ignore.case = TRUE)) {
      "see"
    } else {
      heading_kind
    }

    marked <- rvest::html_elements(item, "[data-phraseid]")
    target <- if (length(marked) > 0) {
      t <- rvest::html_attr(marked[[1]], "data-phrase")
      if (is.na(t) || !nzchar(trimws(t))) rvest::html_text(marked[[1]], trim = TRUE) else t
    } else {
      # "Sub-terms" entries are not always wrapped in a marked span.
      label
    }
    target_id <- if (length(marked) > 0) {
      rvest::html_attr(marked[[1]], "data-phraseid")
    } else {
      NA_character_
    }

    data.frame(xref_kind = kind, xref_target = trimws(target),
               xref_target_id = target_id, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parsed)
  out <- out[!is.na(out$xref_target) & nzchar(out$xref_target), , drop = FALSE]
  if (nrow(out) == 0) return(none)

  kinds <- unique(out$xref_kind[!is.na(out$xref_kind)])
  data.frame(
    xref_kind      = if (length(kinds)) paste(kinds, collapse = " | ") else NA_character_,
    xref_target    = paste(out$xref_target, collapse = " | "),
    xref_target_id = paste(out$xref_target_id, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Collect the IPCC term index
#'
#' @return A data frame with `id`, `term` and `prefix`.
#' @keywords internal
.ipcc_fetch_term_index <- function(pause = 0.3) {
  stubs <- vector("list", length(.IPCC_PREFIXES))

  for (k in seq_along(.IPCC_PREFIXES)) {
    prefix <- .IPCC_PREFIXES[[k]]
    html <- .ipcc_get(paste0(.IPCC_BASE_URL,
                             "/ajax/ajax.searchbyindex.php?q=", prefix))
    if (!is.null(html)) {
      page  <- tryCatch(rvest::read_html(html), error = function(e) NULL)
      nodes <- if (is.null(page)) {
        list()
      } else {
        rvest::html_elements(page, "span.alllink[data-phraseid]")
      }
      if (length(nodes) > 0) {
        ids   <- rvest::html_attr(nodes, "data-phraseid")
        terms <- vapply(rvest::html_text(nodes, trim = TRUE),
                        .ipcc_clean_term, character(1), USE.NAMES = FALSE)
        valid <- !is.na(ids) & nchar(ids) > 0
        stubs[[k]] <- data.frame(id = ids[valid], term = terms[valid],
                                 prefix = prefix, stringsAsFactors = FALSE)
      }
    }
    Sys.sleep(pause)
  }

  out <- do.call(rbind, stubs)
  if (is.null(out)) {
    return(data.frame(id = character(), term = character(),
                      prefix = character(), stringsAsFactors = FALSE))
  }
  out <- out[!duplicated(out$id), , drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Scrape the IPCC glossary from the live website
#'
#' Downloads every term from <https://apps.ipcc.ch/glossary/> at (term, report)
#' granularity, then fills in cross-references for the report entries that have
#' no definition of their own.
#'
#' @param cache_dir Directory to write `ipcc_glossary.csv`.
#' @param progress_callback Optional function `function(current, total, term)`
#'   called after each term is fetched.  Use to drive a Shiny progress bar.
#' @param pause Seconds to wait between requests.
#' @return Invisible path to the written CSV.
#' @export
scrape_ipcc <- function(cache_dir, progress_callback = NULL, pause = 0.3) {
  # Writing UTF-8 text through a non-UTF-8 locale silently mangles it.
  .ensure_utf8_locale()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # ---- Step 1: collect term stubs ------------------------------------------
  term_stubs <- .ipcc_fetch_term_index(pause = pause)
  total <- nrow(term_stubs)
  if (total == 0) {
    stop("No IPCC terms found. The site structure may have changed.")
  }

  # ---- Step 2: per-report definitions ---------------------------------------
  today <- format(Sys.Date())
  all_rows <- vector("list", total)

  for (i in seq_len(total)) {
    stub <- term_stubs[i, ]
    html <- .ipcc_get(paste0(.IPCC_BASE_URL,
                             "/ajax/ajax.searchalloccurance.php?q=",
                             stub$id, "&r="))
    occurrences <- .ipcc_parse_occurrences(html)

    if (nrow(occurrences) == 0) {
      occurrences <- data.frame(report = NA_character_, definition = "",
                                stringsAsFactors = FALSE)
    }

    all_rows[[i]] <- data.frame(
      id             = stub$id,
      prefix         = stub$prefix,
      term           = stub$term,
      report         = occurrences$report,
      definition     = occurrences$definition,
      xref_kind      = NA_character_,
      xref_target    = NA_character_,
      xref_target_id = NA_character_,
      downloaded_at  = today,
      stringsAsFactors = FALSE
    )

    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(i, total, stub$term), error = function(e) NULL)
    }
    Sys.sleep(pause)
  }

  glossary_df <- do.call(rbind, all_rows)
  row.names(glossary_df) <- NULL

  # ---- Step 3: cross-references for entries with no definition --------------
  # These are served only by the per-report endpoint, so they are fetched just
  # for the rows that need them rather than for all 3,000+ pairs.
  glossary_df <- .ipcc_fill_cross_references(
    glossary_df, progress_callback = progress_callback, pause = pause
  )

  out_path <- file.path(cache_dir, "ipcc_glossary.csv")
  utils::write.csv(glossary_df, out_path, row.names = FALSE,
                   fileEncoding = "UTF-8")

  invisible(out_path)
}

#' Fill in related-term pointers from the per-report endpoint
#'
#' @param df A (term, report) data frame as produced by [scrape_ipcc()].
#' @param progress_callback Optional `function(current, total, term)`.
#' @param pause Seconds to wait between requests.
#' @param scope Which rows to fetch. `"all"` covers every (term, report) pair,
#'   since entries that have a definition can still carry `See`, `See Also` or
#'   `Sub-terms` pointers. `"undefined"` restricts the pass to rows with no
#'   definition of their own, which is far cheaper but captures only redirects.
#' @return `df` with `xref_kind`, `xref_target` and `xref_target_id` populated
#'   for the rows that carry pointers.
#' @keywords internal
.ipcc_fill_cross_references <- function(df, progress_callback = NULL,
                                        pause = 0.3,
                                        scope = c("all", "undefined")) {
  scope <- match.arg(scope)

  definition <- trimws(as.character(df$definition))
  definition[is.na(definition)] <- ""
  has_report <- !is.na(df$report) & nzchar(trimws(as.character(df$report)))
  needs <- if (identical(scope, "undefined")) {
    which(!nzchar(definition) & has_report)
  } else {
    which(has_report)
  }
  if (length(needs) == 0) return(df)

  for (k in seq_along(needs)) {
    i <- needs[[k]]
    html <- .ipcc_get(sprintf(
      "%s/ajax/ajax.searchbyphraseandreport.php?q=%s&r=%s",
      .IPCC_BASE_URL, df$id[i], utils::URLencode(trimws(df$report[i])))
    )
    xref <- .ipcc_parse_cross_reference(html)
    df$xref_kind[i]      <- xref$xref_kind
    df$xref_target[i]    <- xref$xref_target
    df$xref_target_id[i] <- xref$xref_target_id

    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(k, length(needs), df$term[i]),
               error = function(e) NULL)
    }
    Sys.sleep(pause)
  }

  df
}
