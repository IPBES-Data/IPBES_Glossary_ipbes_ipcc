# IPCC glossary data loading and scraping
# =============================================================================

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

# Resolve active multilingual IPCC source path (cache first, then bundled snapshot)
resolve_ipcc_multilingual_source_path <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary_multilingual.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary_multilingual.csv")
    }
) {
  cache_file <- file.path(cache_dir, "ipcc_glossary_multilingual.csv")

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
#' @param cache_dir Directory where the user-updated IPCC CSV may be stored.
#'   Defaults to [tools::R_user_dir()] cache.
#' @param bundled_path Path to the bundled IPCC CSV.
#' @return A [dplyr::tibble()] with columns `id`, `term`, `definition`,
#'   `reports` (character, semicolons between report names), `downloaded_at`.
#'   Returns an empty tibble with the correct columns if no data is available.
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

  # Ensure expected columns exist
  for (col in c("id", "term", "definition", "reports", "downloaded_at")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df$definition <- clean_html(df$definition)
  tibble::as_tibble(df[, c("id", "term", "definition", "reports",
                            "downloaded_at")])
}

.empty_ipcc_tibble <- function() {
  tibble::tibble(
    id           = character(),
    term         = character(),
    definition   = character(),
    reports      = character(),
    downloaded_at = character()
  )
}

#' Load the multilingual IPCC glossary matrix
#'
#' Looks for a user-updated multilingual CSV in `cache_dir` first; falls back
#' to the bundled snapshot in `inst/extdata/`.
#'
#' @param cache_dir Directory where the multilingual IPCC CSV may be stored.
#'   Defaults to [tools::R_user_dir()] cache.
#' @param bundled_path Path to the bundled multilingual IPCC CSV.
#' @return A [tibble::tibble()] with one row per `id/report/language`.
#' @export
load_ipcc_multilingual <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary_multilingual.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary_multilingual.csv")
    }
) {
  path <- resolve_ipcc_multilingual_source_path(cache_dir, bundled_path)
  if (!nzchar(path)) {
    return(.empty_ipcc_multilingual_tibble())
  }

  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) {
      message("Could not read multilingual IPCC CSV: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) return(.empty_ipcc_multilingual_tibble())

  expected_cols <- c(
    "id", "report", "language", "term", "definition", "did", "pid",
    "is_translation_available", "is_translation_fetched", "fetch_status",
    "source_endpoint", "downloaded_at"
  )
  for (col in expected_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df$definition <- clean_html(df$definition)
  tibble::as_tibble(df[, expected_cols])
}

.empty_ipcc_multilingual_tibble <- function() {
  tibble::tibble(
    id = character(),
    report = character(),
    language = character(),
    term = character(),
    definition = character(),
    did = character(),
    pid = character(),
    is_translation_available = character(),
    is_translation_fetched = character(),
    fetch_status = character(),
    source_endpoint = character(),
    downloaded_at = character()
  )
}

# =============================================================================

#' Summarise IPCC glossary to one row per term
#'
#' @param ipcc_df Output of [load_ipcc()].
#' @return A [dplyr::tibble()] with columns `term`, `n_reports`,
#'   `summary_definition`, `ipcc_data` (list-column of report x definition
#'   tibbles).
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

  terms <- unique(ipcc_df$term)

  rows <- lapply(terms, function(trm) {
    sub <- ipcc_df[ipcc_df$term == trm, , drop = FALSE]

    # Expand semicolon-separated reports into per-report rows
    detail_rows <- do.call(rbind, lapply(seq_len(nrow(sub)), function(i) {
      rpts <- trimws(strsplit(sub$reports[i], ";\\s*")[[1]])
      rpts <- rpts[nzchar(rpts)]
      if (length(rpts) == 0) rpts <- NA_character_
      data.frame(report     = rpts,
                 definition = sub$definition[i],
                 stringsAsFactors = FALSE)
    }))

    detail <- unique(tibble::as_tibble(detail_rows))

    # Summary: prefer shortest non-trivial
    defs <- sub$definition[!is.na(sub$definition) & nchar(sub$definition) >= 20]
    if (length(defs) == 0) defs <- sub$definition[!is.na(sub$definition)]
    summary_def <- if (length(defs) > 0) defs[which.min(nchar(defs))] else NA_character_

    n_reports <- length(unique(detail$report[!is.na(detail$report)]))

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

# =============================================================================

#' Scrape the IPCC glossary from the live website
#'
#' Adapted from `data-raw/IPCC/download_ipcc_glossary.R`.  Downloads all terms
#' and definitions from <https://apps.ipcc.ch/glossary/> and saves the result
#' to `cache_dir`.
#'
#' @param cache_dir Directory to write `ipcc_glossary.csv`.
#' @param progress_callback Optional function `function(current, total, term)`
#'   called after each term is fetched.  Use to drive a Shiny progress bar.
#' @return Invisible path to the written CSV.
#' @export
scrape_ipcc <- function(cache_dir, progress_callback = NULL) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  base_url <- "https://apps.ipcc.ch/glossary"

  prefixes <- c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
                "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
                "T", "U", "V", "W", "Y", "Z")

  version_tag <- tryCatch(
    if (exists(".package_version_safe", mode = "function")) {
      as.character(.package_version_safe())
    } else {
      "dev"
    },
    error = function(e) "dev"
  )

  session_headers <- httr::add_headers(
    `User-Agent` = paste0("glossary.ipbes.ipcc R package/",
                          version_tag,
                          " (https://github.com/rkrug/glossary_ipbes_ipcc)"),
    `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
  )

  # ---- Helper: clean term name (strip WG suffixes like << WGI,WGII >>) -----
  .clean_term <- function(raw_text) {
    cleaned <- gsub("\\s*\u00ab[^\u00bb]*\u00bb\\s*", "", raw_text)
    cleaned <- gsub("\\s*<<[^>]*>>\\s*", "", cleaned)
    trimws(cleaned)
  }

  # ---- Step 1: collect term stubs ------------------------------------------
  term_stubs <- data.frame(id = character(), term = character(),
                           prefix = character(), stringsAsFactors = FALSE)

  for (prefix in prefixes) {
    # search.php endpoint family (all reports)
    url  <- paste0(base_url, "/ajax/ajax.searchbyindex.php?q=", prefix)
    html <- tryCatch({
      resp <- httr::GET(url, session_headers, httr::timeout(15))
      httr::stop_for_status(resp)
      httr::content(resp, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      message("  Error fetching '", prefix, "': ", conditionMessage(e)); NULL
    })

    if (!is.null(html)) {
      page  <- rvest::read_html(html)
      # IPCC uses data-phraseid on span.alllink elements
      nodes <- rvest::html_elements(page, "span.alllink[data-phraseid]")
      if (length(nodes) > 0) {
        ids   <- rvest::html_attr(nodes, "data-phraseid")
        terms <- vapply(rvest::html_text(nodes, trim = TRUE),
                        .clean_term, character(1), USE.NAMES = FALSE)
        valid <- !is.na(ids) & nchar(ids) > 0
        term_stubs <- rbind(term_stubs,
                            data.frame(id = ids[valid], term = terms[valid],
                                       prefix = prefix, stringsAsFactors = FALSE))
      }
    }
    Sys.sleep(0.3)
  }

  term_stubs <- term_stubs[!duplicated(term_stubs$id), ]
  total <- nrow(term_stubs)

  if (total == 0) {
    stop("No IPCC terms found. The site structure may have changed.")
  }

  # ---- Step 2: fetch full definitions ---------------------------------------
  # The detail endpoint for all reports (search.php family)
  all_rows <- vector("list", total)

  for (i in seq_len(total)) {
    stub <- term_stubs[i, ]
    url  <- paste0(base_url,
                   "/ajax/ajax.searchalloccurance.php?q=",
                   stub$id, "&r=")

    detail_html <- tryCatch({
      resp <- httr::GET(url, session_headers, httr::timeout(15))
      httr::stop_for_status(resp)
      httr::content(resp, as = "text", encoding = "UTF-8")
    }, error = function(e) { NULL })

    definition <- NA_character_
    reports    <- NA_character_

    if (!is.null(detail_html) && nchar(trimws(detail_html)) > 10) {
      page <- rvest::read_html(detail_html)

      # Definitions are in <p> tags within <dd> elements
      def_nodes <- rvest::html_elements(page, "dd p")
      if (length(def_nodes) > 0) {
        definition <- rvest::html_text(def_nodes[[1]], trim = TRUE)
      } else {
        dd_nodes <- rvest::html_elements(page, "dd")
        if (length(dd_nodes) > 0) {
          definition <- rvest::html_text(dd_nodes[[1]], trim = TRUE)
        }
      }

      report_nodes <- rvest::html_elements(page, "[data-report]")
      rpts <- unique(rvest::html_attr(report_nodes, "data-report"))
      rpts <- rpts[!is.na(rpts) & nchar(rpts) > 0]
      reports <- paste(rpts, collapse = "; ")
    }

    all_rows[[i]] <- data.frame(
      id           = stub$id,
      prefix       = stub$prefix,
      term         = stub$term,
      definition   = definition,
      reports      = reports,
      downloaded_at = format(Sys.Date()),
      stringsAsFactors = FALSE
    )

    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(i, total, stub$term), error = function(e) NULL)
    }

    Sys.sleep(0.3)
  }

  # ---- Step 3: save ---------------------------------------------------------
  glossary_df <- do.call(rbind, all_rows)
  out_path    <- file.path(cache_dir, "ipcc_glossary.csv")
  utils::write.csv(glossary_df, out_path, row.names = FALSE, fileEncoding = "UTF-8")

  invisible(out_path)
}

# =============================================================================
# Multilingual IPCC scraping helpers
# =============================================================================

.ipcc_multilingual_languages <- function() {
  c("en", "ar", "es", "fr", "ru", "zh")
}

.ipcc_multilingual_prefixes <- function() {
  c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
    "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "Y", "Z")
}

.ipcc_clean_term_name <- function(raw_text) {
  cleaned <- gsub("\\s*\u00ab[^\u00bb]*\u00bb\\s*", "", raw_text)
  cleaned <- gsub("\\s*<<[^>]*>>\\s*", "", cleaned)
  cleaned <- gsub("\\s*\\([^)]*\\)\\s*$", "", cleaned)
  trimws(cleaned)
}

.ipcc_fetch_html <- function(url, headers, timeout_sec = 20) {
  resp <- httr::GET(url, headers, httr::timeout(timeout_sec))
  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "UTF-8")
}

.ipcc_collect_term_stubs <- function(base_url, headers, prefixes, endpoint) {
  stubs <- data.frame(
    id = character(),
    term = character(),
    prefix = character(),
    endpoint = character(),
    stringsAsFactors = FALSE
  )

  for (prefix in prefixes) {
    url <- paste0(base_url, "/ajax/", endpoint, "?q=", utils::URLencode(prefix, reserved = TRUE))
    html <- tryCatch(.ipcc_fetch_html(url, headers), error = function(e) NULL)
    if (is.null(html) || !nzchar(trimws(html))) next

    page <- rvest::read_html(html)
    nodes <- rvest::html_elements(page, "span.alllink[data-phraseid]")
    if (length(nodes) == 0) next

    ids <- rvest::html_attr(nodes, "data-phraseid")
    terms <- vapply(rvest::html_text(nodes, trim = TRUE), .ipcc_clean_term_name,
                    character(1), USE.NAMES = FALSE)
    valid <- !is.na(ids) & nzchar(ids)
    if (!any(valid)) next

    stubs <- rbind(
      stubs,
      data.frame(
        id = ids[valid],
        term = terms[valid],
        prefix = prefix,
        endpoint = endpoint,
        stringsAsFactors = FALSE
      )
    )
    Sys.sleep(0.15)
  }

  if (nrow(stubs) == 0) return(stubs)

  stubs <- stubs[order(stubs$id, stubs$endpoint), , drop = FALSE]
  stubs <- stubs[!duplicated(stubs$id), , drop = FALSE]
  rownames(stubs) <- NULL
  stubs
}

.ipcc_parse_report_codes_from_occurrence_html <- function(detail_html) {
  if (is.null(detail_html) || !nzchar(trimws(detail_html))) return(character(0))
  page <- rvest::read_html(detail_html)
  btns <- rvest::html_elements(page, "button.specificlink[data-report]")
  reports <- unique(trimws(rvest::html_attr(btns, "data-report")))
  reports <- reports[!is.na(reports) & nzchar(reports)]
  reports
}

.ipcc_parse_detail_payload <- function(detail_html) {
  out <- list(
    term = NA_character_,
    definition = NA_character_,
    translations = data.frame(
      language = character(),
      did = character(),
      pid = character(),
      report = character(),
      stringsAsFactors = FALSE
    )
  )

  if (is.null(detail_html) || !nzchar(trimws(detail_html))) return(out)

  page <- rvest::read_html(detail_html)

  h5 <- rvest::html_elements(page, "h5")
  if (length(h5) > 0) {
    t <- rvest::html_text(h5[[1]], trim = TRUE)
    out$term <- .ipcc_clean_term_name(t)
  }

  p_nodes <- rvest::html_elements(page, "dd p")
  if (length(p_nodes) > 0) {
    out$definition <- rvest::html_text(p_nodes[[1]], trim = TRUE)
  } else {
    dd_nodes <- rvest::html_elements(page, "dd")
    if (length(dd_nodes) > 0) {
      out$definition <- rvest::html_text(dd_nodes[[1]], trim = TRUE)
    }
  }

  tr_nodes <- rvest::html_elements(page, "a.translation[data-lang][data-did][data-pid][data-report]")
  if (length(tr_nodes) > 0) {
    tr <- data.frame(
      language = trimws(rvest::html_attr(tr_nodes, "data-lang")),
      did = trimws(rvest::html_attr(tr_nodes, "data-did")),
      pid = trimws(rvest::html_attr(tr_nodes, "data-pid")),
      report = trimws(rvest::html_attr(tr_nodes, "data-report")),
      stringsAsFactors = FALSE
    )
    tr <- tr[!is.na(tr$language) & nzchar(tr$language), , drop = FALSE]
    if (nrow(tr) > 0) {
      tr <- tr[!duplicated(paste(tr$language, tr$did, tr$pid, tr$report, sep = "::")), , drop = FALSE]
      out$translations <- tr
    }
  }

  out
}

.ipcc_parse_translation_payload <- function(translation_html, lang) {
  out <- list(term = NA_character_, definition = NA_character_)
  if (is.null(translation_html) || !nzchar(trimws(translation_html))) return(out)

  page <- rvest::read_html(translation_html)
  h5 <- rvest::html_elements(page, "h5")
  if (length(h5) > 0) {
    term <- rvest::html_text(h5[[1]], trim = TRUE)
    term <- gsub(sprintf("\\s*\\(%s\\)\\s*$", lang), "", term)
    out$term <- .ipcc_clean_term_name(term)
  }

  p_nodes <- rvest::html_elements(page, "dd p")
  if (length(p_nodes) > 0) {
    out$definition <- rvest::html_text(p_nodes[[1]], trim = TRUE)
  } else {
    dd_nodes <- rvest::html_elements(page, "dd")
    if (length(dd_nodes) > 0) {
      out$definition <- rvest::html_text(dd_nodes[[1]], trim = TRUE)
    }
  }

  out
}

.as_yes_no <- function(x) ifelse(isTRUE(x), "TRUE", "FALSE")

#' Scrape multilingual IPCC glossary matrix from live endpoints
#'
#' Builds a matrix with one row per `id/report/language`, where languages are
#' `en`, `ar`, `es`, `fr`, `ru`, `zh`. Missing translations are persisted as
#' explicit placeholder rows.
#'
#' @param cache_dir Directory to write `ipcc_glossary_multilingual.csv`.
#' @param progress_callback Optional callback `function(current, total, label)`.
#' @return Invisible path to written CSV.
scrape_ipcc_multilingual <- function(cache_dir, progress_callback = NULL) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  base_url <- "https://apps.ipcc.ch/glossary"
  prefixes <- .ipcc_multilingual_prefixes()
  languages <- .ipcc_multilingual_languages()

  version_tag <- tryCatch(
    if (exists(".package_version_safe", mode = "function")) {
      as.character(.package_version_safe())
    } else {
      "dev"
    },
    error = function(e) "dev"
  )

  session_headers <- httr::add_headers(
    `User-Agent` = paste0("glossary.ipbes.ipcc multilingual scraper/",
                          version_tag,
                          " (https://github.com/rkrug/glossary_ipbes_ipcc)"),
    `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
  )

  stubs_a <- .ipcc_collect_term_stubs(
    base_url = base_url,
    headers = session_headers,
    prefixes = prefixes,
    endpoint = "ajax.searchbyindex.php"
  )
  stubs_b <- .ipcc_collect_term_stubs(
    base_url = base_url,
    headers = session_headers,
    prefixes = prefixes,
    endpoint = "ajax.searchbylatestindex.php"
  )
  term_stubs <- rbind(stubs_a, stubs_b)
  if (nrow(term_stubs) == 0) {
    stop("No IPCC terms found for multilingual scrape. Site structure may have changed.")
  }
  term_stubs <- term_stubs[order(term_stubs$id, term_stubs$endpoint), , drop = FALSE]
  term_stubs <- term_stubs[!duplicated(term_stubs$id), , drop = FALSE]
  rownames(term_stubs) <- NULL

  rows <- vector("list", 0L)
  total <- nrow(term_stubs)
  downloaded_at <- format(Sys.Date())

  for (i in seq_len(total)) {
    stub <- term_stubs[i, ]
    occ_url <- paste0(
      base_url, "/ajax/ajax.searchalloccurance.php?q=",
      utils::URLencode(stub$id, reserved = TRUE), "&r="
    )
    occ_html <- tryCatch(.ipcc_fetch_html(occ_url, session_headers), error = function(e) NULL)
    reports <- .ipcc_parse_report_codes_from_occurrence_html(occ_html)
    if (length(reports) == 0) reports <- NA_character_

    for (report in reports) {
      report_is_missing <- is.na(report) || !nzchar(report)
      report_val <- if (report_is_missing) NA_character_ else report

      detail_html <- if (report_is_missing) {
        NULL
      } else {
        detail_url <- paste0(
          base_url, "/ajax/ajax.searchbyphraseandreport.php?q=",
          utils::URLencode(stub$id, reserved = TRUE),
          "&r=", utils::URLencode(report, reserved = TRUE)
        )
        tryCatch(.ipcc_fetch_html(detail_url, session_headers), error = function(e) NULL)
      }

      detail <- .ipcc_parse_detail_payload(detail_html)
      tr_map <- detail$translations

      for (lang in languages) {
        if (identical(lang, "en")) {
          fetched <- !is.na(detail$definition) && nzchar(detail$definition)
          rows[[length(rows) + 1L]] <- data.frame(
            id = stub$id,
            report = report_val,
            language = lang,
            term = if (!is.na(detail$term) && nzchar(detail$term)) detail$term else stub$term,
            definition = detail$definition,
            did = NA_character_,
            pid = NA_character_,
            is_translation_available = .as_yes_no(TRUE),
            is_translation_fetched = .as_yes_no(fetched),
            fetch_status = if (report_is_missing) "missing_report" else if (fetched) "ok" else "english_detail_missing",
            source_endpoint = "ajax.searchbyphraseandreport.php",
            downloaded_at = downloaded_at,
            stringsAsFactors = FALSE
          )
          next
        }

        tr_row <- tr_map[tr_map$language == lang & tr_map$report == report_val, , drop = FALSE]
        if (nrow(tr_row) == 0) {
          tr_row <- tr_map[tr_map$language == lang, , drop = FALSE]
        }

        if (nrow(tr_row) == 0) {
          rows[[length(rows) + 1L]] <- data.frame(
            id = stub$id,
            report = report_val,
            language = lang,
            term = NA_character_,
            definition = NA_character_,
            did = NA_character_,
            pid = NA_character_,
            is_translation_available = .as_yes_no(FALSE),
            is_translation_fetched = .as_yes_no(FALSE),
            fetch_status = if (report_is_missing) "missing_report" else "not_available",
            source_endpoint = "ajax.searchbyphraseandreport.php",
            downloaded_at = downloaded_at,
            stringsAsFactors = FALSE
          )
          next
        }

        tr_row <- tr_row[1, , drop = FALSE]
        did <- tr_row$did[[1]]
        pid <- tr_row$pid[[1]]

        tr_url <- paste0(
          base_url, "/ajax/ajax.gettranslation.php?q=",
          utils::URLencode(did, reserved = TRUE),
          "&p=", utils::URLencode(pid, reserved = TRUE),
          "&r=", utils::URLencode(report_val, reserved = TRUE),
          "&l=", utils::URLencode(lang, reserved = TRUE)
        )

        tr_html <- tryCatch(.ipcc_fetch_html(tr_url, session_headers), error = function(e) NULL)
        tr_payload <- .ipcc_parse_translation_payload(tr_html, lang)
        fetched <- !is.null(tr_html) &&
          ((!is.na(tr_payload$definition) && nzchar(tr_payload$definition)) ||
             (!is.na(tr_payload$term) && nzchar(tr_payload$term)))

        rows[[length(rows) + 1L]] <- data.frame(
          id = stub$id,
          report = report_val,
          language = lang,
          term = tr_payload$term,
          definition = tr_payload$definition,
          did = did,
          pid = pid,
          is_translation_available = .as_yes_no(TRUE),
          is_translation_fetched = .as_yes_no(fetched),
          fetch_status = if (fetched) "ok" else "translation_fetch_failed_or_empty",
          source_endpoint = "ajax.gettranslation.php",
          downloaded_at = downloaded_at,
          stringsAsFactors = FALSE
        )
        Sys.sleep(0.1)
      }
    }

    if (!is.null(progress_callback)) {
      label <- paste0(stub$term, " [", i, "/", total, "]")
      tryCatch(progress_callback(i, total, label), error = function(e) NULL)
    }
    Sys.sleep(0.1)
  }

  if (length(rows) == 0) {
    out <- .empty_ipcc_multilingual_tibble()
  } else {
    out <- do.call(rbind, rows)
    out <- out[!duplicated(out[, c("id", "report", "language")]), , drop = FALSE]
    out <- tibble::as_tibble(out)
  }

  out_path <- file.path(cache_dir, "ipcc_glossary_multilingual.csv")
  utils::write.csv(out, out_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_path)
}
