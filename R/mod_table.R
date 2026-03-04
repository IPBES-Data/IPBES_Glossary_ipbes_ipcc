# Shiny module: main glossary comparison table
# =============================================================================

#' UI for the glossary comparison table module
#'
#' @param id Shiny module namespace id.
#' @return A `reactable` output wrapped in a `div`.
#' @keywords internal
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("glossary_table"))
}

# =============================================================================

#' Server for the glossary comparison table module
#'
#' @param id        Shiny module namespace id.
#' @param merged_rv A [shiny::reactiveVal()] containing the merged glossary
#'   [tibble::tibble()] (output of [merge_glossaries()]).
#' @param cache_dir Path to the cache directory.
#' @param highlight_terms_rv Optional [shiny::reactiveVal()] containing a
#'   character vector of terms to highlight in the table.
#' @param filter_terms_rv Optional reactive returning a character vector of
#'   terms; when provided, only these terms are shown in the table.
#' @keywords internal
mod_table_server <- function(
    id,
    merged_rv,
    cache_dir,
    highlight_terms_rv = NULL,
    filter_terms_rv = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    prepared_rv <- shiny::reactiveVal(NULL)
    detail_registered <- new.env(parent = emptyenv())

    shiny::observeEvent(merged_rv(), {
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) {
        prepared_rv(data)
      } else {
        prepared_rv(.prepare_table_data(data))
      }
    }, ignoreNULL = FALSE)

    # -- Sorted data -----------------------------------------------------------
    display_data <- shiny::reactive({
      data <- prepared_rv()
      if (is.null(data) || nrow(data) == 0) return(data)
      if (!is.null(filter_terms_rv)) {
        filter_terms <- as.character(filter_terms_rv())
        filter_terms <- unique(filter_terms[!is.na(filter_terms) & nzchar(filter_terms)])
        data <- data[data$matched_term %in% filter_terms, , drop = FALSE]
      }
      if (is.null(data) || nrow(data) == 0) return(data)

      highlight_terms <- if (!is.null(highlight_terms_rv)) {
        as.character(highlight_terms_rv())
      } else {
        character(0)
      }
      highlight_terms <- unique(highlight_terms[!is.na(highlight_terms) & nzchar(highlight_terms)])

      is_highlight <- data$matched_term %in% highlight_terms
      highlight_order <- match(data$matched_term, highlight_terms)
      highlight_order[is.na(highlight_order)] <- length(highlight_terms) + seq_len(sum(is.na(highlight_order)))

      data[order(!is_highlight, highlight_order, data$matched_term, na.last = TRUE), ]
    })

    # -- Reactable output ------------------------------------------------------
    output$glossary_table <- reactable::renderReactable({

      data <- display_data()
      highlight_terms <- if (!is.null(highlight_terms_rv)) {
        as.character(highlight_terms_rv())
      } else {
        character(0)
      }

      if (is.null(data) || nrow(data) == 0) {
        msg <- if (!is.null(filter_terms_rv)) {
          "No selected tree terms. Click a graph node to populate this table."
        } else {
          "No data available. Run data-raw/prepare_data.R first."
        }
        return(reactable::reactable(
          data.frame(Message = msg),
          bordered = TRUE
        ))
      }

      # Strip list-columns for the display frame; keep indices for detail fn
      display_df <- data[, c("term_display",
                              "matched_term",
                              "sim_between_all", "between_similarity_html",
                              "sim_within_ipbes", "ipbes_preview_html",
                              "sim_within_ipcc", "ipcc_preview_html"),
                         drop = FALSE]

      reactable::reactable(
        display_df,
        onClick      = "expand",
        searchable   = TRUE,
        pagination   = TRUE,
        defaultPageSize = 100,
        striped      = TRUE,
        highlight    = TRUE,
        resizable    = TRUE,
        bordered     = TRUE,
        rowStyle = function(index) {
          term <- display_df$matched_term[index]
          if (length(highlight_terms) > 0 && term %in% highlight_terms) {
            list(
              background = "#fff7d6",
              boxShadow = "inset 4px 0 0 #f59e0b"
            )
          } else {
            NULL
          }
        },
        theme        = reactable::reactableTheme(
          cellStyle     = list(fontSize = "1.02rem"),
          stripedColor  = "#f8f8f8",
          highlightColor = "#f0f4ff"
        ),
        columns = list(
          term_display = reactable::colDef(
            name    = "Term",
            minWidth = 160,
            sortable = TRUE,
            html     = FALSE
          ),
          matched_term = reactable::colDef(show = FALSE),
          sim_between_all = reactable::colDef(
            name     = "Similarity",
            width    = 260,
            sortable = TRUE,
            html     = TRUE,
            cell     = function(value, index) display_df$between_similarity_html[index]
          ),
          between_similarity_html = reactable::colDef(show = FALSE),
          sim_within_ipbes = reactable::colDef(
            name     = "IPBES Definition",
            minWidth = 260,
            sortable = TRUE,
            align    = "left",
            html     = TRUE,
            cell     = function(value, index) display_df$ipbes_preview_html[index]
          ),
          ipbes_preview_html = reactable::colDef(show = FALSE),
          sim_within_ipcc = reactable::colDef(
            name     = "IPCC Definition",
            minWidth = 260,
            sortable = TRUE,
            align    = "left",
            html     = TRUE,
            cell     = function(value, index) display_df$ipcc_preview_html[index]
          ),
          ipcc_preview_html = reactable::colDef(show = FALSE)
        ),
        details = function(index) {
          shiny::uiOutput(session$ns(paste0("detail_row_", index)))
        }
      )
    })

    shiny::observe({
      data <- display_data()
      if (is.null(data) || nrow(data) == 0) return(invisible(NULL))

      for (idx in seq_len(nrow(data))) {
        key <- paste0("detail_row_", idx)
        if (exists(key, envir = detail_registered, inherits = FALSE)) next

        local({
          i <- idx
          output[[key]] <- shiny::renderUI({
            row <- display_data()[i, , drop = FALSE]
            .detail_row_html(row)
          })
          shiny::outputOptions(output, key, suspendWhenHidden = TRUE)
        })
        assign(key, TRUE, envir = detail_registered)
      }
    })
  })
}

# =============================================================================
# Helper: build the expandable detail row HTML

.detail_row_html <- function(row) {
  ipbes_data <- row$ipbes_data[[1]]
  ipcc_data  <- row$ipcc_data[[1]]
  ipbes_grouped <- .group_definitions_by_text(ipbes_data, "assessment")
  ipcc_grouped  <- .group_definitions_by_text(ipcc_data, "report")

  # ---- Pairwise matrix section ----------------------------------------------
  matrix_section <- .pairwise_matrix_section(ipbes_grouped, ipcc_grouped)

  htmltools::div(class = "detail-row", matrix_section)
}

.pairwise_matrix_section <- function(
    ipbes_grouped,
    ipcc_grouped,
    similarity_min = 0.08
) {
  items <- .build_pairwise_items(ipbes_grouped, ipcc_grouped)
  n <- nrow(items)

  if (n < 2) {
    return(htmltools::div(
      class = "detail-diff",
      htmltools::h4("Pairwise Similarity Matrix"),
      htmltools::p(style = "color:#777; margin:0;",
                   "Not enough grouped definitions to build a matrix.")
    ))
  }

  head_cells <- c(
    list(htmltools::tags$th(class = "pair-matrix-corner", "")),
    lapply(seq_len(n), function(j) {
      htmltools::tags$th(class = "pair-matrix-axis", items$axis[j])
    })
  )

  body_rows <- lapply(seq_len(n), function(i) {
    row_cells <- list(
      htmltools::tags$th(class = "pair-matrix-axis pair-matrix-rowhead", items$axis[i])
    )

    for (j in seq_len(n)) {
      if (j <= i) {
        row_cells <- c(row_cells, list(
          htmltools::tags$td(class = "pair-matrix-empty", "")
        ))
        next
      }

      sim <- compute_text_similarity(items$definition[i], items$definition[j])
      if (is.na(sim)) sim <- 0
      bg <- .matrix_sim_color(sim)

      diff_block <- if (sim >= similarity_min) {
        render_text_diff(
          items$definition[i],
          items$definition[j],
          label_a = items$short_axis[i],
          label_b = items$short_axis[j]
        )
      } else {
        htmltools::div(
          class = "pair-matrix-low",
          paste0("Too different (< ", round(similarity_min * 100, 0), "%).")
        )
      }

      row_cells <- c(row_cells, list(
        htmltools::tags$td(
          class = "pair-matrix-cell",
          style = paste0("background:", bg, ";"),
          htmltools::div(
            class = "pair-matrix-score",
            paste0(round(sim * 100, 1), "%")
          ),
          diff_block
        )
      ))
    }

    do.call(htmltools::tags$tr, row_cells)
  })

  htmltools::div(
    class = "detail-diff",
    htmltools::h4("Pairwise Similarity Matrix"),
    htmltools::div(
      class = "pair-matrix-note",
      "Order: IPBES groups first, then IPCC groups. Lower triangle is intentionally empty."
    ),
    htmltools::div(
      class = "pair-matrix-wrap",
      htmltools::tags$table(
        class = "pair-matrix",
        htmltools::tags$thead(do.call(htmltools::tags$tr, head_cells)),
        htmltools::tags$tbody(body_rows)
      )
    )
  )
}

.TABLE_VIEW_CACHE_VERSION <- 4L

.has_current_table_view_cache <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(TRUE)
  precomputed_cols <- c(
    "term_display", "ipbes_preview_html", "ipcc_preview_html",
    "between_similarity_html"
  )
  if (!all(precomputed_cols %in% names(data))) return(FALSE)
  if (!"table_view_version" %in% names(data)) return(FALSE)
  all(!is.na(data$table_view_version)) &&
    all(as.integer(data$table_view_version) == .TABLE_VIEW_CACHE_VERSION)
}

.prepare_table_data <- function(data) {
  if (.has_current_table_view_cache(data)) {
    return(data)
  }

  n <- nrow(data)

  data$term_display <- vapply(seq_len(n), function(i) {
    .term_with_counts(
      term    = data$matched_term[i],
      ipbes_n = data$ipbes_n_assessments[i],
      ipcc_n  = data$ipcc_n_reports[i]
    )
  }, character(1))

  data$ipbes_preview_html <- vapply(seq_len(n), function(i) {
    .definition_preview_html(
      detail_df   = data$ipbes_data[[i]],
      source_col  = "assessment",
      similarity_score = data$sim_within_ipbes[i],
      empty_label = "\u2014"
    )
  }, character(1))

  data$ipcc_preview_html <- vapply(seq_len(n), function(i) {
    .definition_preview_html(
      detail_df   = data$ipcc_data[[i]],
      source_col  = "report",
      similarity_score = data$sim_within_ipcc[i],
      empty_label = "\u2014"
    )
  }, character(1))

  data$between_similarity_html <- vapply(seq_len(n), function(i) {
    .between_similarity_html(data$sim_between_all[i])
  }, character(1))
  data$table_view_version <- rep(.TABLE_VIEW_CACHE_VERSION, n)

  data
}

.term_with_counts <- function(term, ipbes_n, ipcc_n) {
  counts <- character(0)
  if (!is.na(ipbes_n) && ipbes_n > 0) counts <- c(counts, paste0("IPBES \u00d7", ipbes_n))
  if (!is.na(ipcc_n) && ipcc_n > 0) counts <- c(counts, paste0("IPCC \u00d7", ipcc_n))
  if (length(counts) == 0) return(term)
  paste0(term, " (", paste(counts, collapse = " | "), ")")
}

.definition_preview_html <- function(
    detail_df,
    source_col,
    similarity_score,
    empty_label = "\u2014"
) {
  sim_html <- .within_similarity_html(similarity_score)
  grouped <- .group_definitions_by_text(detail_df, source_col)
  if (is.null(grouped) || nrow(grouped) == 0) {
    return(
      htmltools::div(
        class = "def-preview-wrap",
        htmltools::HTML(sim_html),
        htmltools::span(style = "color:#aaa;", empty_label)
      ) |> as.character()
    )
  }

  blocks <- vapply(seq_len(nrow(grouped)), function(i) {
    src <- grouped[[source_col]][i]
    def <- grouped$definition[i]
    src_html <- if (!is.na(src) && nzchar(src)) {
      src_parts <- strsplit(as.character(src), "\n", fixed = TRUE)[[1]]
      src_parts <- src_parts[nzchar(trimws(src_parts))]
      src_line <- paste(src_parts, collapse = "; ")
      paste0(
        "<div class=\"def-preview-source\"><strong>",
        htmltools::htmlEscape(src_line),
        "</strong></div>"
      )
    } else {
      ""
    }
    paste0(
      "<div class=\"def-preview-item\">",
      src_html,
      "<div class=\"def-preview-text\">", htmltools::htmlEscape(def), "</div>",
      "</div>"
    )
  }, character(1))

  htmltools::div(
    class = "def-preview-wrap",
    htmltools::HTML(sim_html),
    htmltools::div(
      class = "def-preview",
      htmltools::HTML(paste(blocks, collapse = ""))
    )
  ) |> as.character()
}

.within_similarity_html <- function(score) {
  htmltools::div(
    class = "def-sim-row",
    htmltools::HTML(similarity_bar_html(score))
  ) |> as.character()
}

.between_similarity_html <- function(score) {
  similarity_bar_html(score)
}

.group_definitions_by_text <- function(detail_df, source_col) {
  .empty_grouped <- function() {
    out <- data.frame(character(0), character(0), stringsAsFactors = FALSE)
    names(out) <- c(source_col, "definition")
    tibble::as_tibble(out)
  }

  if (is.null(detail_df) || nrow(detail_df) == 0 || !"definition" %in% names(detail_df)) {
    return(.empty_grouped())
  }

  definitions <- trimws(as.character(detail_df$definition))
  keep <- !is.na(definitions) & nzchar(definitions)
  if (!any(keep)) {
    return(.empty_grouped())
  }

  detail_df <- detail_df[keep, , drop = FALSE]
  definitions <- trimws(as.character(detail_df$definition))
  sources <- if (source_col %in% names(detail_df)) {
    trimws(as.character(detail_df[[source_col]]))
  } else {
    rep(NA_character_, length(definitions))
  }

  rows <- list()
  for (i in seq_along(definitions)) {
    def <- definitions[i]
    src <- sources[i]
    pos <- match(def, vapply(rows, `[[`, character(1), "definition"))

    if (is.na(pos)) {
      rows[[length(rows) + 1]] <- list(
        definition = def,
        sources    = if (!is.na(src) && nzchar(src)) src else character(0)
      )
    } else if (!is.na(src) && nzchar(src) && !src %in% rows[[pos]]$sources) {
      rows[[pos]]$sources <- c(rows[[pos]]$sources, src)
    }
  }

  out <- data.frame(
    sources    = vapply(rows, function(r) paste(r$sources, collapse = "\n"), character(1)),
    definition = vapply(rows, `[[`, character(1), "definition"),
    stringsAsFactors = FALSE
  )
  names(out)[1] <- source_col
  tibble::as_tibble(out)
}

.multiline_html_escape <- function(text) {
  parts <- strsplit(as.character(text), "\n", fixed = TRUE)[[1]]
  paste(htmltools::htmlEscape(parts), collapse = "<br/>")
}

.multiline_cell_html <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return(htmltools::span(style = "color:#aaa;", "\u2014") |> as.character())
  }
  htmltools::HTML(.multiline_html_escape(value))
}

.source_inline <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return("\u2014")
  }
  parts <- strsplit(as.character(value), "\n", fixed = TRUE)[[1]]
  parts <- parts[nzchar(trimws(parts))]
  if (length(parts) == 0) "\u2014" else paste(parts, collapse = "; ")
}

.build_pairwise_items <- function(ipbes_grouped, ipcc_grouped) {
  ip_n <- if (!is.null(ipbes_grouped)) nrow(ipbes_grouped) else 0
  ic_n <- if (!is.null(ipcc_grouped)) nrow(ipcc_grouped) else 0

  ip_axis <- if (ip_n > 0) {
    vapply(seq_len(ip_n), function(i) {
      paste0("IPBES ", i, ": ", .source_inline(ipbes_grouped$assessment[i]))
    }, character(1))
  } else character(0)
  ic_axis <- if (ic_n > 0) {
    vapply(seq_len(ic_n), function(i) {
      paste0("IPCC ", i, ": ", .source_inline(ipcc_grouped$report[i]))
    }, character(1))
  } else character(0)

  ip_short <- if (ip_n > 0) paste0("IPBES ", seq_len(ip_n)) else character(0)
  ic_short <- if (ic_n > 0) paste0("IPCC ", seq_len(ic_n)) else character(0)

  defs <- c(
    if (ip_n > 0) ipbes_grouped$definition else character(0),
    if (ic_n > 0) ipcc_grouped$definition else character(0)
  )

  data.frame(
    axis       = c(ip_axis, ic_axis),
    short_axis = c(ip_short, ic_short),
    definition = defs,
    stringsAsFactors = FALSE
  )
}

.matrix_sim_color <- function(score) {
  s <- if (is.na(score)) 0 else max(0, min(1, score))
  hue <- round(120 * s)
  sprintf("hsl(%d, 85%%, 90%%)", hue)
}
