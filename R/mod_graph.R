# Shiny module: directed term hierarchy graph
# =============================================================================

#' UI for the hierarchy graph module
#'
#' @param id Shiny module namespace id.
#' @return UI tags.
#' @keywords internal
mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  tip_label <- function(text, tip) {
    htmltools::tags$span(text, title = tip)
  }

  shiny::tagList(
    htmltools::div(
      class = "graph-controls-panel",
      shiny::fluidRow(
        shiny::column(
          width = 7,
          htmltools::div(
            style = "max-width: 720px;",
            htmltools::div(
              title = "Choose one hierarchy tree to display in the graph. You can type to filter tree names.",
              shiny::selectizeInput(
                inputId = ns("tree_selector"),
                label = tip_label(
                  "Select Tree",
                  "Choose one hierarchy tree to display. Type to filter available trees."
                ),
                choices = c(),
                selected = NULL,
                options = list(
                  placeholder = "Type to filter trees...",
                  searchField = c("text", "value"),
                  maxOptions = 2000,
                  closeAfterSelect = TRUE,
                  selectOnTab = TRUE
                )
              )
            ),
            htmltools::div(
              style = "margin-top: -6px;",
              title = "Switch between alphabetical order and descending tree size for the selector choices.",
              shiny::checkboxInput(
                inputId = ns("tree_sort_alpha"),
                label = tip_label(
                  "Sort trees alphabetically",
                  "When checked, tree selector entries are sorted alphabetically. Otherwise they are sorted by node count."
                ),
                value = FALSE
              )
            ),
            htmltools::div(
              style = "margin-top: 2px;",
              shiny::div(
                class = "graph-focus-nav",
                shiny::actionButton(
                  inputId = ns("focus_prev_tree"),
                  label = "Focus Previous Tree",
                  icon = shiny::icon("arrow-left"),
                  class = "btn btn-default btn-sm",
                  title = "Select the previous tree based on the current selector ordering."
                ),
                shiny::actionButton(
                  inputId = ns("focus_next_tree"),
                  label = "Focus Next Tree",
                  icon = shiny::icon("arrow-right"),
                  class = "btn btn-default btn-sm",
                  title = "Select the next tree based on the current selector ordering."
                ),
                shiny::actionButton(
                  inputId = ns("reset_view"),
                  label = "Reset View",
                  icon = shiny::icon("crosshairs"),
                  class = "btn btn-default btn-sm",
                  title = "Reset pan and zoom to fit the currently selected tree."
                )
              )
            )
          )
        ),
        shiny::column(
          width = 5,
          shiny::div(
            style = "max-width: 520px; margin-left: auto;",
            title = "Hide weaker parent-child edges by increasing the minimum directed subsumption score threshold.",
            shiny::sliderInput(
              inputId = ns("min_score"),
              label = tip_label(
                "Minimum subsumption score",
                "Edges are shown only if score >= this value. Higher values keep only stronger parent-child links."
              ),
              min = 0.45,
              max = 0.95,
              value = 0.65,
              step = 0.01
            ),
            htmltools::div(
              style = "margin-top: -6px;",
              title = "If checked, each child keeps only its strongest parent edge at the current threshold.",
              shiny::checkboxInput(
                inputId = ns("best_parent_only"),
                label = tip_label(
                  "Keep only best parent per child",
                  "Keep only the top-scoring incoming edge per child term."
                ),
                value = TRUE
              )
            ),
            htmltools::div(
              class = "graph-export-controls",
              title = "Export current graph view, settings, and tables as HTML or PDF.",
              shiny::selectInput(
                inputId = ns("export_format"),
                label = tip_label(
                  "Export format",
                  "Choose HTML for offline viewing, or PDF for print-ready output."
                ),
                choices = c(
                  "HTML (offline)" = "html",
                  "PDF (print)" = "pdf"
                ),
                selected = "html",
                width = "220px"
              ),
              shiny::downloadButton(
                outputId = ns("export_report"),
                label = "Export",
                class = "btn btn-default btn-sm"
              )
            )
          )
        )
      )
    ),
    htmltools::div(
      class = "graph-summary-row",
      htmltools::div(
        class = "graph-tree-badge-wrap",
        shiny::uiOutput(ns("graph_tree_badge"))
      )
    ),
    shiny::uiOutput(ns("graph_widget")),
    shiny::tabsetPanel(
      id = ns("graph_tables_view"),
      type = "tabs",
      shiny::tabPanel(
        title = "Top Directed Edges",
        reactable::reactableOutput(ns("hierarchy_table"))
      ),
      shiny::tabPanel(
        title = "Glossary Table",
        mod_table_ui(ns("selected_tree_table"))
      )
    )
  )
}

# =============================================================================

#' Server for the hierarchy graph module
#'
#' @param id Shiny module namespace id.
#' @param merged_rv ReactiveVal with merged glossary data.
#' @param cache_dir Path to app cache directory.
#' @param highlight_terms_rv Optional [shiny::reactiveVal()] used to push graph
#'   selection highlights to other modules.
#' @keywords internal
mod_graph_server <- function(id, merged_rv, cache_dir, highlight_terms_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    full_edges_rv <- shiny::reactiveVal(NULL)
    cache_meta_rv <- shiny::reactiveVal(NULL)
    def_lookup_rv <- shiny::reactiveVal(character(0))
    source_lookup_rv <- shiny::reactiveVal(character(0))
    selected_node_rv <- shiny::reactiveVal(NULL)
    selected_tree_rv <- shiny::reactiveVal("")
    last_view_rv <- shiny::reactiveVal(NULL)
    restore_view_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(merged_rv(), {
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) {
        cache_meta_rv(NULL)
        full_edges_rv(.empty_hierarchy_edges())
        def_lookup_rv(character(0))
        source_lookup_rv(character(0))
        selected_node_rv(NULL)
        selected_tree_rv("")
        if (!is.null(highlight_terms_rv)) highlight_terms_rv(character(0))
        return(invisible(NULL))
      }

      def_lookup_rv(.build_definition_lookup(data))
      source_lookup_rv(.build_term_source_lookup(data))

      meta <- .hierarchy_cache_meta(data)
      cache_meta_rv(meta)
      cached <- .load_hierarchy_cache(cache_dir, meta)
      if (is.null(cached)) {
        cached <- .load_packaged_hierarchy_cache(meta)
        if (!is.null(cached)) {
          .save_hierarchy_cache(cache_dir, meta, cached)
        }
      }
      if (!is.null(cached)) {
        full_edges_rv(cached)
      } else {
        full_edges_rv(NULL)
      }
    }, ignoreNULL = FALSE)

    full_hierarchy_edges <- shiny::reactive({
      edges <- full_edges_rv()
      if (!is.null(edges)) return(edges)

      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) return(.empty_hierarchy_edges())

      withCallingHandlers({
        shiny::withProgress(message = "Computing hierarchy graph cache...", value = 0.1, {
          edges <- compute_term_hierarchy(
            merged_data = data,
            min_score = 0,
            best_parent_only = FALSE
          )
          shiny::incProgress(0.8)
          full_edges_rv(edges)
          meta <- cache_meta_rv()
          if (!is.null(meta)) {
            .save_hierarchy_cache(cache_dir, meta, edges)
          }
          shiny::incProgress(0.1)
          edges
        })
      }, warning = function(w) {
        # keep graph computation resilient if cache writes fail
        invokeRestart("muffleWarning")
      })
    })

    hierarchy_edges <- shiny::reactive({
      edges <- full_hierarchy_edges()
      if (is.null(edges) || nrow(edges) == 0) return(.empty_hierarchy_edges())

      edges <- edges[edges$score >= input$min_score, , drop = FALSE]
      if (nrow(edges) == 0) return(edges)
      if (isTRUE(input$best_parent_only)) {
        edges <- .select_best_parent_per_child(edges)
      }
      # Guard vis hierarchical layout from low-threshold cycles.
      edges <- .ensure_acyclic_edges(edges)
      edges[order(-edges$score, edges$child_term), , drop = FALSE]
    })

    tree_catalog <- shiny::reactive({
      .ordered_roots_with_nodes(hierarchy_edges())
    })

    tree_selector_catalog <- shiny::reactive({
      edges <- hierarchy_edges()
      trees <- tree_catalog()
      roots <- as.character(trees$roots)
      node_sets <- trees$node_sets
      sizes <- if (length(node_sets) > 0) {
        vapply(node_sets, length, integer(1))
      } else {
        integer(0)
      }

      selected_anchor <- selected_tree_rv()
      if (!is.null(selected_anchor) && nzchar(selected_anchor) && !(selected_anchor %in% roots)) {
        anchor_nodes <- if (nrow(edges) > 0) {
          .hierarchy_connected_terms(edges, selected_anchor)
        } else {
          character(0)
        }
        roots <- c(roots, selected_anchor)
        node_sets <- c(node_sets, list(anchor_nodes))
        sizes <- c(sizes, length(anchor_nodes))
      }

      if (length(roots) == 0) {
        return(list(roots = character(0), node_sets = list(), sizes = integer(0)))
      }

      if (isTRUE(input$tree_sort_alpha)) {
        ord <- order(tolower(roots), roots)
      } else {
        ord <- order(-sizes, roots)
      }
      list(
        roots = roots[ord],
        node_sets = node_sets[ord],
        sizes = sizes[ord]
      )
    })

    selected_tree_root <- shiny::reactive({
      trees <- tree_selector_catalog()
      roots <- trees$roots
      if (length(roots) == 0) return("")
      anchor <- selected_tree_rv()
      if (!is.null(anchor) && nzchar(anchor)) {
        if (anchor %in% roots) return(anchor)
      }
      roots[[1]]
    })

    selected_tree_terms <- shiny::reactive({
      trees <- tree_selector_catalog()
      root <- selected_tree_root()
      if (!nzchar(root) || length(trees$roots) == 0) return(character(0))
      idx <- match(root, trees$roots)
      if (is.na(idx)) return(character(0))
      trees$node_sets[[idx]]
    })

    plot_edges <- shiny::reactive({
      edges <- hierarchy_edges()
      if (nrow(edges) == 0) return(edges)
      selected_terms <- selected_tree_terms()
      if (length(selected_terms) == 0) return(edges[0, , drop = FALSE])
      edges <- edges[
        edges$parent_term %in% selected_terms &
          edges$child_term %in% selected_terms,
        ,
        drop = FALSE
      ]
      edges[order(-edges$score, edges$child_term), , drop = FALSE]
    })

    shiny::observeEvent(input$node_click, {
      node <- input$node_click
      if (is.null(node) || !nzchar(node)) {
        selected_node_rv(NULL)
      } else {
        selected_node_rv(as.character(node))
      }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$graph_view, {
      v <- input$graph_view
      if (is.null(v)) return(invisible(NULL))
      x <- suppressWarnings(as.numeric(v$x))
      y <- suppressWarnings(as.numeric(v$y))
      s <- suppressWarnings(as.numeric(v$scale))
      if (is.na(x) || is.na(y) || is.na(s) || s <= 0) return(invisible(NULL))
      last_view_rv(list(x = x, y = y, scale = s))
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$min_score, {
      restore_view_rv(last_view_rv())
    }, ignoreInit = TRUE)

    shiny::observe({
      edges <- plot_edges()
      selected <- selected_node_rv()
      valid_nodes <- unique(c(edges$parent_term, edges$child_term))
      if (!is.null(selected) && nzchar(selected) && !(selected %in% valid_nodes)) {
        selected_node_rv(NULL)
      }
    })

    shiny::observe({
      trees <- tree_selector_catalog()
      roots <- trees$roots
      sizes <- trees$sizes
      if (length(roots) == 0) {
        shiny::updateSelectizeInput(
          session,
          "tree_selector",
          choices = c("No trees available" = ""),
          selected = "",
          server = TRUE
        )
        return(invisible(NULL))
      }

      labels <- vapply(seq_along(roots), function(i) {
        paste0(roots[[i]], " (", sizes[[i]], " nodes)")
      }, character(1))
      choices <- stats::setNames(roots, labels)

      selected <- selected_tree_root()
      shiny::updateSelectizeInput(
        session,
        "tree_selector",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    })

    shiny::observeEvent(input$focus_prev_tree, {
      trees <- tree_selector_catalog()
      roots <- trees$roots
      n <- length(roots)
      if (n == 0) return(invisible(NULL))
      root <- selected_tree_root()
      idx <- match(root, roots)
      if (is.na(idx)) idx <- 1L
      target <- max(1L, idx - 1L)
      selected_tree_rv(roots[[target]])
    })

    shiny::observeEvent(input$focus_next_tree, {
      trees <- tree_selector_catalog()
      roots <- trees$roots
      n <- length(roots)
      if (n == 0) return(invisible(NULL))
      root <- selected_tree_root()
      idx <- match(root, roots)
      if (is.na(idx)) idx <- 1L
      target <- min(n, idx + 1L)
      selected_tree_rv(roots[[target]])
    })

    shiny::observeEvent(input$reset_view, {
      restore_view_rv(NULL)
      last_view_rv(NULL)
      if (!requireNamespace("visNetwork", quietly = TRUE)) return(invisible(NULL))
      linked <- selected_tree_terms()
      proxy <- visNetwork::visNetworkProxy(session$ns("hierarchy_graph"), session = session)
      tryCatch({
        if (length(linked) > 0) {
          visNetwork::visFit(
            proxy,
            nodes = linked,
            animation = list(duration = 400, easingFunction = "easeInOutQuad")
          )
        } else {
          visNetwork::visFit(
            proxy,
            animation = list(duration = 400, easingFunction = "easeInOutQuad")
          )
        }
      }, error = function(e) {
        invisible(NULL)
      })
    })

    shiny::observeEvent(input$tree_selector, {
      roots <- tree_selector_catalog()$roots
      if (!is.null(input$tree_selector) &&
          nzchar(input$tree_selector) &&
          input$tree_selector %in% roots) {
        selected_tree_rv(input$tree_selector)
      }
      root <- selected_tree_root()
      if (!nzchar(root)) {
        selected_node_rv(NULL)
      } else if (!identical(selected_node_rv(), root)) {
        selected_node_rv(root)
      }
    }, ignoreInit = TRUE)

    mod_table_server(
      id = "selected_tree_table",
      merged_rv = merged_rv,
      cache_dir = cache_dir,
      highlight_terms_rv = highlight_terms_rv,
      filter_terms_rv = selected_tree_terms
    )

    shiny::observe({
      if (is.null(highlight_terms_rv)) return(invisible(NULL))
      selected <- selected_node_rv()
      if (is.null(selected) || !nzchar(selected)) {
        highlight_terms_rv(character(0))
        return(invisible(NULL))
      }

      in_tree <- selected %in% selected_tree_terms()
      if (isTRUE(in_tree)) {
        highlight_terms_rv(selected)
      } else {
        highlight_terms_rv(character(0))
      }
    })

    output$graph_tree_badge <- shiny::renderUI({
      root <- selected_tree_root()
      shown_edges <- plot_edges()
      terms <- selected_tree_terms()

      if (!nzchar(root)) {
        return(
          htmltools::span(
            class = "graph-tree-badge graph-tree-badge-empty",
            "Tree: none"
          )
        )
      }

      terms <- unique(terms[!is.na(terms) & nzchar(terms)])
      node_count <- length(unique(c(root, terms)))
      edge_count <- if (is.null(shown_edges)) 0L else nrow(shown_edges)

      htmltools::span(
        class = "graph-tree-badge",
        htmltools::tags$strong("Tree:"),
        " ",
        htmltools::span(class = "graph-tree-name", root),
        " | ",
        htmltools::tags$strong("Nodes:"),
        " ",
        as.character(node_count),
        " | ",
        htmltools::tags$strong("Edges:"),
        " ",
        as.character(edge_count)
      )
    })

    output$graph_widget <- shiny::renderUI({
      if (requireNamespace("visNetwork", quietly = TRUE)) {
        visNetwork::visNetworkOutput(session$ns("hierarchy_graph"), height = "760px")
      } else {
        shiny::plotOutput(session$ns("hierarchy_plot"), height = "760px")
      }
    })

    if (requireNamespace("visNetwork", quietly = TRUE)) {
      graph_data <- shiny::reactive({
        .build_vis_graph_data(plot_edges(), def_lookup_rv(), source_lookup_rv())
      })

      output$hierarchy_graph <- visNetwork::renderVisNetwork({
        .build_vis_hierarchy_network(
          graph_data = graph_data(),
          click_input_id = session$ns("node_click"),
          view_input_id = session$ns("graph_view")
        )
      })

      shiny::observeEvent(graph_data(), {
        view <- restore_view_rv()
        if (is.null(view)) return(invisible(NULL))
        proxy <- visNetwork::visNetworkProxy(session$ns("hierarchy_graph"), session = session)
        move_to_fn <- if (exists("visMoveTo", envir = asNamespace("visNetwork"), mode = "function", inherits = FALSE)) {
          get("visMoveTo", envir = asNamespace("visNetwork"))
        } else {
          NULL
        }
        tryCatch({
          if (is.function(move_to_fn)) {
            do.call(move_to_fn, list(
              graph = proxy,
              position = list(x = view$x, y = view$y),
              scale = view$scale,
              animation = list(duration = 0)
            ))
          } else {
            # Fallback for older visNetwork versions without visMoveTo export.
            linked <- selected_tree_terms()
            if (length(linked) > 0) {
              visNetwork::visFit(proxy, nodes = linked, animation = list(duration = 0))
            } else {
              visNetwork::visFit(proxy, animation = list(duration = 0))
            }
          }
        }, error = function(e) {
          invisible(NULL)
        })
        restore_view_rv(NULL)
      }, ignoreInit = TRUE)

      shiny::observe({
        gd <- graph_data()
        if (is.null(gd) || nrow(gd$nodes) == 0) return(invisible(NULL))
        style <- .vis_style_for_selection(gd, selected_node_rv())
        if (nrow(style$nodes) == 0 && nrow(style$edges) == 0) return(invisible(NULL))
        proxy <- visNetwork::visNetworkProxy(session$ns("hierarchy_graph"), session = session)
        tryCatch({
          if (nrow(style$nodes) > 0) {
            proxy <- visNetwork::visUpdateNodes(proxy, nodes = style$nodes)
          }
          if (nrow(style$edges) > 0) {
            proxy <- visNetwork::visUpdateEdges(proxy, edges = style$edges)
          }
        }, error = function(e) {
          invisible(NULL)
        })
      })
    } else {
      output$hierarchy_plot <- shiny::renderPlot({
        edges <- plot_edges()
        .plot_hierarchy_network(edges)
      }, res = 110)
    }

    output$hierarchy_table <- reactable::renderReactable({
      edges <- plot_edges()
      selected <- selected_node_rv()
      linked <- selected_tree_terms()

      if (length(linked) > 0) {
        edges <- edges[edges$parent_term %in% linked & edges$child_term %in% linked, , drop = FALSE]
      } else {
        edges <- edges[0, , drop = FALSE]
      }

      if (nrow(edges) == 0) {
        msg <- if (length(linked) == 0) {
          "No selected tree is available at this threshold."
        } else {
          "No directed edges available for the selected tree at this threshold."
        }
        return(reactable::reactable(
          data.frame(Message = msg),
          bordered = TRUE
        ))
      }
      is_highlight <- if (!is.null(selected) && nzchar(selected)) {
        edges$parent_term == selected | edges$child_term == selected
      } else {
        rep(FALSE, nrow(edges))
      }
      edges <- edges[order(!is_highlight, -edges$score, edges$parent_term, edges$child_term), , drop = FALSE]
      k <- min(50L, nrow(edges))
      out <- edges[seq_len(k), c(
        "parent_term", "child_term", "score", "lex_sub", "def_contain", "def_sim"
      )]
      reactable::reactable(
        out,
        striped = TRUE,
        bordered = TRUE,
        defaultPageSize = 100,
        rowStyle = function(index) {
          row <- out[index, , drop = FALSE]
          if (!is.null(selected) && nzchar(selected) &&
              (row$parent_term == selected || row$child_term == selected)) {
            return(list(background = "#ffe8b3", boxShadow = "inset 4px 0 0 #f59e0b"))
          }
          NULL
        },
        columns = list(
          parent_term = reactable::colDef(name = "Parent"),
          child_term = reactable::colDef(name = "Child"),
          score = reactable::colDef(name = "Score", format = reactable::colFormat(digits = 3)),
          lex_sub = reactable::colDef(name = "LexSub", format = reactable::colFormat(digits = 3)),
          def_contain = reactable::colDef(name = "DefContain", format = reactable::colFormat(digits = 3)),
          def_sim = reactable::colDef(name = "DefSim", format = reactable::colFormat(digits = 3))
        )
      )
    })

    export_payload <- shiny::reactive({
      selected_terms <- unique(selected_tree_terms())
      selected_terms <- selected_terms[!is.na(selected_terms) & nzchar(selected_terms)]
      graph_export <- .build_vis_graph_data(plot_edges(), def_lookup_rv(), source_lookup_rv())

      edges <- plot_edges()
      edge_df <- if (is.null(edges) || nrow(edges) == 0) {
        data.frame(
          Parent = character(0),
          Child = character(0),
          Score = numeric(0),
          LexSub = numeric(0),
          DefContain = numeric(0),
          DefSim = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Parent = as.character(edges$parent_term),
          Child = as.character(edges$child_term),
          Score = round(as.numeric(edges$score), 4),
          LexSub = round(as.numeric(edges$lex_sub), 4),
          DefContain = round(as.numeric(edges$def_contain), 4),
          DefSim = round(as.numeric(edges$def_sim), 4),
          stringsAsFactors = FALSE
        )
      }

      merged <- merged_rv()
      glossary_df <- data.frame(
        Term = character(0),
        Within_IPBES = character(0),
        Within_IPCC = character(0),
        Between_All = character(0),
        IPBES_Assessments = character(0),
        IPBES_Definitions = character(0),
        IPCC_Reports = character(0),
        IPCC_Definitions = character(0),
        stringsAsFactors = FALSE
      )

      if (!is.null(merged) && nrow(merged) > 0 && length(selected_terms) > 0) {
        selected <- merged[merged$matched_term %in% selected_terms, , drop = FALSE]
        selected <- selected[order(selected$matched_term), , drop = FALSE]

        if (nrow(selected) > 0) {
          rows <- lapply(seq_len(nrow(selected)), function(i) {
            row <- selected[i, , drop = FALSE]

            ipbes_grouped <- .group_definitions_by_text(row$ipbes_data[[1]], "assessment")
            ipcc_grouped <- .group_definitions_by_text(row$ipcc_data[[1]], "report")

            data.frame(
              Term = as.character(row$matched_term),
              Within_IPBES = .export_fmt_score(row$sim_within_ipbes),
              Within_IPCC = .export_fmt_score(row$sim_within_ipcc),
              Between_All = .export_fmt_score(row$sim_between_all),
              IPBES_Assessments = .export_flatten_grouped_sources(ipbes_grouped, "assessment"),
              IPBES_Definitions = .export_flatten_grouped_defs(ipbes_grouped),
              IPCC_Reports = .export_flatten_grouped_sources(ipcc_grouped, "report"),
              IPCC_Definitions = .export_flatten_grouped_defs(ipcc_grouped),
              stringsAsFactors = FALSE
            )
          })
          glossary_df <- do.call(rbind, rows)
        }
      }

      settings <- list(
        `Selected Tree` = selected_tree_root(),
        `Selected Node` = selected_node_rv(),
        `Displayed Nodes` = length(selected_terms),
        `Displayed Edges` = nrow(edge_df),
        `Minimum Subsumption Score` = sprintf("%.2f", input$min_score),
        `Best Parent Only` = if (isTRUE(input$best_parent_only)) "Yes" else "No",
        `Sort Trees Alphabetically` = if (isTRUE(input$tree_sort_alpha)) "Yes" else "No"
      )

      list(
        settings = settings,
        edges = edge_df,
        glossary = glossary_df,
        graph_nodes = graph_export$nodes,
        graph_edges = graph_export$edges
      )
    })

    output$export_report <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$export_format, "pdf")) "pdf" else "html"
        paste0("glossary-export-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".", ext)
      },
      content = function(file) {
        if (!requireNamespace("rmarkdown", quietly = TRUE)) {
          stop("Package 'rmarkdown' is required for export.")
        }

        is_pdf <- identical(input$export_format, "pdf")
        if (is_pdf && !nzchar(Sys.which("pdflatex"))) {
          stop("PDF export requires a LaTeX engine (e.g., TinyTeX). Please install it or export as HTML.")
        }

        payload <- export_payload()
        template <- .export_report_template_path()
        if (!file.exists(template)) {
          stop("Export template not found: ", template)
        }

        tmp_dir <- tempfile("glossary_export_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

        graph_png <- file.path(tmp_dir, "graph.png")
        grDevices::png(filename = graph_png, width = 2400, height = 1400, res = 170)
        tryCatch(
          .plot_hierarchy_network(plot_edges()),
          finally = grDevices::dev.off()
        )

        output_name <- if (is_pdf) "export.pdf" else "export.html"
        output_format <- if (is_pdf) "pdf_document" else "html_document"

        rendered <- rmarkdown::render(
          input = template,
          output_format = output_format,
          output_file = output_name,
          output_dir = tmp_dir,
          params = list(
            settings = payload$settings,
            graph_png = graph_png,
            edges_df = payload$edges,
            glossary_df = payload$glossary,
            graph_nodes = payload$graph_nodes,
            graph_edges = payload$graph_edges,
            generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          ),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
        )

        ok <- file.copy(rendered, file, overwrite = TRUE)
        if (!isTRUE(ok)) {
          stop("Could not write exported report.")
        }
      }
    )
  })
}

# ---- Plot helpers -----------------------------------------------------------

.plot_hierarchy_network <- function(edges) {
  graphics::plot.new()

  if (is.null(edges) || nrow(edges) == 0) {
    graphics::text(0.5, 0.5, "No hierarchy edges to display.", cex = 1.1, col = "#666666")
    return(invisible(NULL))
  }

  nodes <- .layout_hierarchy_nodes(edges)
  if (nrow(nodes) == 0) {
    graphics::text(0.5, 0.5, "No hierarchy nodes to display.", cex = 1.1, col = "#666666")
    return(invisible(NULL))
  }

  edge_idx <- cbind(
    match(edges$parent_term, nodes$term),
    match(edges$child_term, nodes$term)
  )

  xlim <- range(nodes$x) + c(-0.08, 0.08)
  ylim <- range(nodes$y) + c(-0.12, 0.12)
  graphics::plot.window(xlim = xlim, ylim = ylim)
  graphics::axis(1, labels = FALSE, tick = FALSE)
  graphics::axis(2, labels = FALSE, tick = FALSE)
  graphics::box()

  # Draw direction edges (parent -> child).
  score_rng <- range(edges$score, na.rm = TRUE)
  denom <- if (diff(score_rng) == 0) 1 else diff(score_rng)
  edge_lwd <- 0.7 + 2.2 * ((edges$score - score_rng[1]) / denom)
  edge_col <- grDevices::adjustcolor("#6c757d", alpha.f = 0.55)

  for (i in seq_len(nrow(edges))) {
    p <- edge_idx[i, 1]
    c <- edge_idx[i, 2]
    if (is.na(p) || is.na(c)) next
    graphics::arrows(
      x0 = nodes$x[p], y0 = nodes$y[p],
      x1 = nodes$x[c], y1 = nodes$y[c],
      length = 0.065, angle = 22,
      lwd = edge_lwd[i],
      col = edge_col
    )
  }

  level_vals <- sort(unique(nodes$level))
  level_col <- stats::setNames(
    grDevices::hcl.colors(length(level_vals), "Teal"),
    as.character(level_vals)
  )
  node_col <- unname(level_col[as.character(nodes$level)])

  graphics::points(
    nodes$x, nodes$y,
    pch = 21, bg = node_col, col = "#1f2937", cex = 1.15, lwd = 0.8
  )

  label_cex <- if (nrow(nodes) > 120) 0.45 else 0.62
  labels <- .shorten_term_label(nodes$term)
  graphics::text(nodes$x, nodes$y, labels = labels, pos = 3, cex = label_cex, col = "#1f2937")

  graphics::title(
    main = "Directed Term Hierarchy (parent \u2192 child)",
    sub = "Edges ranked by subsumption score. Layers follow term token count."
  )
}

.layout_hierarchy_nodes <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(data.frame(term = character(), level = integer(), x = numeric(), y = numeric()))
  }

  parent_levels <- data.frame(
    term = edges$parent_term,
    level = edges$parent_token_n,
    stringsAsFactors = FALSE
  )
  child_levels <- data.frame(
    term = edges$child_term,
    level = edges$child_token_n,
    stringsAsFactors = FALSE
  )
  node_levels <- rbind(parent_levels, child_levels)
  node_levels <- stats::aggregate(level ~ term, data = node_levels, FUN = min)
  node_levels <- node_levels[order(node_levels$level, node_levels$term), , drop = FALSE]
  if (nrow(node_levels) == 0) return(node_levels)

  level_values <- sort(unique(node_levels$level))
  y_map <- stats::setNames(
    seq(from = 0.95, to = 0.05, length.out = length(level_values)),
    as.character(level_values)
  )

  node_levels$x <- NA_real_
  node_levels$y <- NA_real_
  for (lv in level_values) {
    idx <- which(node_levels$level == lv)
    k <- length(idx)
    xs <- if (k == 1L) 0.5 else seq(from = 0.06, to = 0.94, length.out = k)
    node_levels$x[idx] <- xs
    node_levels$y[idx] <- y_map[[as.character(lv)]]
  }

  node_levels
}

.shorten_term_label <- function(x, max_nchar = 32L) {
  ifelse(
    nchar(x) <= max_nchar,
    x,
    paste0(substr(x, 1, max_nchar - 1L), "\u2026")
  )
}

.build_definition_lookup <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(character(0))

  terms <- as.character(data$matched_term)
  terms <- terms[!is.na(terms)]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]
  terms <- unique(terms)
  if (length(terms) == 0) return(character(0))

  defs <- stats::setNames(rep("", length(terms)), terms)
  for (term in terms) {
    idx <- which(data$matched_term == term)[1]
    row <- data[idx, , drop = FALSE]
    defs[[term]] <- .collect_term_definition_text(row)
  }
  defs
}

.build_term_source_lookup <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(character(0))

  out <- character(0)
  for (i in seq_len(nrow(data))) {
    term <- as.character(data$matched_term[[i]])
    term <- trimws(term)
    if (is.na(term) || !nzchar(term)) next

    has_ipbes <- FALSE
    has_ipcc <- FALSE

    if ("ipbes_concept" %in% names(data)) {
      v <- as.character(data$ipbes_concept[[i]])
      has_ipbes <- !is.na(v) && nzchar(trimws(v))
    }
    if (!has_ipbes && "ipbes_data" %in% names(data) && length(data$ipbes_data) >= i) {
      d <- data$ipbes_data[[i]]
      has_ipbes <- !is.null(d) && nrow(d) > 0
    }

    if ("ipcc_term" %in% names(data)) {
      v <- as.character(data$ipcc_term[[i]])
      has_ipcc <- !is.na(v) && nzchar(trimws(v))
    }
    if (!has_ipcc && "ipcc_data" %in% names(data) && length(data$ipcc_data) >= i) {
      d <- data$ipcc_data[[i]]
      has_ipcc <- !is.null(d) && nrow(d) > 0
    }

    source <- if (has_ipbes && has_ipcc) {
      "both"
    } else if (has_ipbes) {
      "ipbes"
    } else if (has_ipcc) {
      "ipcc"
    } else {
      "unknown"
    }

    existing <- if (term %in% names(out)) unname(out[[term]]) else ""
    if (!nzchar(existing) || identical(existing, "unknown")) {
      out[[term]] <- source
    } else if (!identical(source, "unknown") && !identical(existing, source)) {
      out[[term]] <- "both"
    }
  }

  out
}

.source_shape <- function(source) {
  source <- tolower(trimws(as.character(source)))
  out <- rep("dot", length(source))
  out[source == "ipbes"] <- "triangle"
  out[source == "ipcc"] <- "square"
  out[source == "both"] <- "diamond"
  out
}

.source_label <- function(source) {
  source <- tolower(trimws(as.character(source)))
  out <- rep("Unknown", length(source))
  out[source == "ipbes"] <- "IPBES"
  out[source == "ipcc"] <- "IPCC"
  out[source == "both"] <- "IPBES + IPCC"
  out
}

.build_vis_graph_data <- function(edges, definition_lookup, source_lookup = character(0)) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(list(
      nodes = data.frame(
        id = character(),
        label = character(),
        title = character(),
        group = character(),
        shape = character(),
        level = integer(),
        value = numeric(),
        color.background = character(),
        color.border = character(),
        font.color = character(),
        stringsAsFactors = FALSE
      ),
      edges = data.frame(
        id = character(),
        from = character(),
        to = character(),
        width = numeric(),
        title = character(),
        color.color = character(),
        color.opacity = numeric(),
        stringsAsFactors = FALSE
      ),
      edge_source = edges
    ))
  }

  node_terms <- unique(c(edges$parent_term, edges$child_term))

  parent_levels <- data.frame(term = edges$parent_term, level = edges$parent_token_n, stringsAsFactors = FALSE)
  child_levels <- data.frame(term = edges$child_term, level = edges$child_token_n, stringsAsFactors = FALSE)
  levels_df <- stats::aggregate(level ~ term, data = rbind(parent_levels, child_levels), FUN = min)
  levels_df <- levels_df[match(node_terms, levels_df$term), , drop = FALSE]

  # Keep top-level trees ordered left-to-right by decreasing tree size;
  # ties are resolved alphabetically by root term.
  node_terms <- .order_terms_by_root_priority(edges, node_terms, levels_df)
  levels_df <- levels_df[match(node_terms, levels_df$term), , drop = FALSE]

  deg_tab <- table(c(edges$parent_term, edges$child_term))
  node_degree <- as.numeric(deg_tab[node_terms])
  node_degree[is.na(node_degree)] <- 1

  level_vals <- sort(unique(levels_df$level))
  level_cols <- stats::setNames(
    grDevices::hcl.colors(length(level_vals), "Teal"),
    as.character(level_vals)
  )

  roots <- setdiff(unique(edges$parent_term), unique(edges$child_term))
  if (length(roots) == 0) roots <- unique(edges$parent_term)
  is_root <- node_terms %in% roots
  node_source <- if (!is.null(source_lookup) && length(source_lookup) > 0) {
    unname(source_lookup[node_terms])
  } else {
    rep(NA_character_, length(node_terms))
  }
  node_source[is.na(node_source) | !nzchar(node_source)] <- "unknown"
  node_source_label <- .source_label(node_source)

  node_title <- vapply(seq_along(node_terms), function(i) {
    term <- node_terms[[i]]
    definition <- if (!is.null(definition_lookup) && term %in% names(definition_lookup)) {
      definition_lookup[[term]]
    } else {
      ""
    }
    definition <- gsub("\\s+", " ", definition)
    definition <- trimws(definition)
    if (!nzchar(definition)) definition <- "No definition available."
    if (nchar(definition) > 1000) {
      definition <- paste0(substr(definition, 1, 997), "...")
    }
    paste0(
      "<div style='max-width:460px; white-space:normal;'>",
      "<strong>", htmltools::htmlEscape(term), "</strong><br/>",
      "<span><strong>Source:</strong> ", htmltools::htmlEscape(node_source_label[[i]]), "</span><br/>",
      "<span>", htmltools::htmlEscape(definition), "</span>",
      "</div>"
    )
  }, character(1))

  nodes <- data.frame(
    id = node_terms,
    label = node_terms,
    title = node_title,
    group = node_source_label,
    shape = .source_shape(node_source),
    level = levels_df$level,
    value = ifelse(is_root, pmax(70, pmin(110, 10 + node_degree)), pmin(40, 6 + node_degree)),
    color.background = unname(level_cols[as.character(levels_df$level)]),
    color.border = "#334155",
    font.color = "#111827",
    font.size = ifelse(is_root, 36, 32),
    font.vadjust = ifelse(is_root, -140, 0),
    stringsAsFactors = FALSE
  )

  width_scale <- {
    rng <- range(edges$score, na.rm = TRUE)
    if (diff(rng) == 0) rep(2.2, nrow(edges)) else 1 + 5 * ((edges$score - rng[1]) / diff(rng))
  }

  edge_title <- paste0(
    "<b>", htmltools::htmlEscape(edges$parent_term), " \u2192 ",
    htmltools::htmlEscape(edges$child_term), "</b><br/>",
    "Score: ", sprintf("%.3f", edges$score),
    "<br/>LexSub: ", sprintf("%.3f", edges$lex_sub),
    "<br/>DefContain: ", sprintf("%.3f", edges$def_contain),
    "<br/>DefSim: ", sprintf("%.3f", edges$def_sim)
  )

  edge_df <- data.frame(
    id = paste0("e", seq_len(nrow(edges))),
    from = edges$parent_term,
    to = edges$child_term,
    width = width_scale,
    title = edge_title,
    color.color = "#6b7280",
    color.opacity = 0.60,
    stringsAsFactors = FALSE
  )

  list(
    nodes = nodes,
    edges = edge_df,
    edge_source = edges
  )
}

.order_terms_by_root_priority <- function(edges, node_terms, levels_df) {
  if (is.null(edges) || nrow(edges) == 0 || length(node_terms) == 0) {
    return(node_terms)
  }

  trees <- .ordered_roots_with_nodes(edges)
  roots <- trees$roots
  root_nodes <- trees$node_sets
  if (length(roots) == 0) return(node_terms)

  priority <- stats::setNames(rep(length(roots) + 1L, length(node_terms)), node_terms)
  for (i in seq_along(roots)) {
    covered <- root_nodes[[i]]
    matched <- intersect(node_terms, covered)
    if (length(matched) == 0) next
    priority[matched] <- pmin(priority[matched], i)
  }

  node_df <- data.frame(
    term = node_terms,
    level = levels_df$level[match(node_terms, levels_df$term)],
    priority = as.integer(priority[node_terms]),
    stringsAsFactors = FALSE
  )
  node_df <- node_df[order(node_df$priority, node_df$level, node_df$term), , drop = FALSE]
  node_df$term
}

.ordered_roots_with_nodes <- function(edges) {
  empty <- list(roots = character(0), node_sets = list())
  if (is.null(edges) || nrow(edges) == 0) return(empty)

  roots <- setdiff(unique(edges$parent_term), unique(edges$child_term))
  if (length(roots) == 0) roots <- unique(edges$parent_term)
  roots <- sort(as.character(roots))
  if (length(roots) == 0) return(empty)

  child_of <- split(edges$child_term, edges$parent_term)
  node_sets <- lapply(roots, function(root) unique(c(root, .graph_reachable(root, child_of))))
  sizes <- vapply(node_sets, length, integer(1))
  ord <- order(-sizes, roots)

  list(
    roots = roots[ord],
    node_sets = node_sets[ord]
  )
}

.build_vis_hierarchy_network <- function(
    graph_data,
    click_input_id = "node_click",
    view_input_id = "graph_view"
) {
  if (is.null(graph_data) || nrow(graph_data$nodes) == 0) {
    nodes <- data.frame(
      id = "none",
      label = "No edges at current threshold",
      title = "No hierarchy edges pass current filters.",
      stringsAsFactors = FALSE
    )
    return(
      visNetwork::visNetwork(nodes, data.frame(), width = "100%", height = "760px") |>
        visNetwork::visNodes(shape = "box", color = list(background = "#f3f4f6", border = "#9ca3af")) |>
        visNetwork::visPhysics(enabled = FALSE)
    )
  }

  visNetwork::visNetwork(
    nodes = graph_data$nodes,
    edges = graph_data$edges,
    width = "100%",
    height = "760px"
  ) |>
    visNetwork::visHierarchicalLayout(
      direction = "UD",
      sortMethod = "directed",
      levelSeparation = 230,
      nodeSpacing = 180,
      treeSpacing = 220,
      blockShifting = FALSE,
      edgeMinimization = FALSE,
      parentCentralization = TRUE
    ) |>
    visNetwork::visNodes(
      font = list(size = 18, face = "Segoe UI")
    ) |>
    visNetwork::visGroups(
      groupname = "IPBES",
      shape = "triangle"
    ) |>
    visNetwork::visGroups(
      groupname = "IPCC",
      shape = "square"
    ) |>
    visNetwork::visGroups(
      groupname = "IPBES + IPCC",
      shape = "diamond"
    ) |>
    visNetwork::visGroups(
      groupname = "Unknown",
      shape = "dot"
    ) |>
    visNetwork::visLegend(
      useGroups = TRUE,
      position = "right",
      main = "Node Shapes",
      zoom = FALSE
    ) |>
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      smooth = list(
        enabled = TRUE,
        type = "cubicBezier",
        forceDirection = "vertical",
        roundness = 0.16
      )
    ) |>
    visNetwork::visInteraction(
      hover = TRUE,
      navigationButtons = TRUE,
      zoomView = TRUE,
      dragView = TRUE,
      dragNodes = TRUE
    ) |>
    visNetwork::visPhysics(enabled = FALSE) |>
    visNetwork::visEvents(
      click = sprintf(
        "function(params) {
           var pos = this.getViewPosition();
           Shiny.setInputValue('%s', {
             x: pos.x, y: pos.y, scale: this.getScale(), nonce: Date.now()
           }, {priority: 'event'});
           if (params.nodes && params.nodes.length > 0) {
             Shiny.setInputValue('%s', params.nodes[0], {priority: 'event'});
           } else {
             Shiny.setInputValue('%s', '', {priority: 'event'});
           }
         }",
        view_input_id,
        click_input_id,
        click_input_id
      ),
      dragEnd = sprintf(
        "function(params) {
           var pos = this.getViewPosition();
           Shiny.setInputValue('%s', {
             x: pos.x, y: pos.y, scale: this.getScale(), nonce: Date.now()
           }, {priority: 'event'});
         }",
        view_input_id
      ),
      zoom = sprintf(
        "function(params) {
           var pos = this.getViewPosition();
           Shiny.setInputValue('%s', {
             x: pos.x, y: pos.y, scale: this.getScale(), nonce: Date.now()
           }, {priority: 'event'});
         }",
        view_input_id
      )
    )
}

.vis_style_for_selection <- function(graph_data, selected_node = NULL) {
  if (is.null(graph_data) || nrow(graph_data$nodes) == 0) {
    return(list(
      nodes = data.frame(id = character(), stringsAsFactors = FALSE),
      edges = data.frame(id = character(), stringsAsFactors = FALSE)
    ))
  }

  nodes <- graph_data$nodes[, c(
    "id", "value", "color.background", "color.border", "font.color", "font.size", "font.vadjust"
  ), drop = FALSE]
  edges <- graph_data$edges[, c("id", "width", "color.color", "color.opacity"), drop = FALSE]

  if (is.null(selected_node) || !nzchar(selected_node) || !(selected_node %in% nodes$id)) {
    return(list(nodes = nodes, edges = edges))
  }

  linked_terms <- .hierarchy_connected_terms(graph_data$edge_source, selected_node)
  linked_edge <- graph_data$edges$from %in% linked_terms & graph_data$edges$to %in% linked_terms

  non_linked <- !(nodes$id %in% linked_terms)
  nodes$color.background[non_linked] <- "#e5e7eb"
  nodes$color.border[non_linked] <- "#9ca3af"
  nodes$font.color[non_linked] <- "#9ca3af"

  sel <- nodes$id == selected_node
  nodes$color.background[sel] <- "#f59e0b"
  nodes$color.border[sel] <- "#b45309"
  nodes$font.color[sel] <- "#111827"
  nodes$value[sel] <- pmax(nodes$value[sel], 20)

  edges$color.color <- ifelse(linked_edge, "#2563eb", "#c7cbd1")
  edges$color.opacity <- ifelse(linked_edge, 0.95, 0.18)

  list(nodes = nodes, edges = edges)
}

.hierarchy_connected_terms <- function(edges, root_term) {
  if (is.null(edges) || nrow(edges) == 0 || is.null(root_term) || !nzchar(root_term)) {
    return(character(0))
  }
  all_nodes <- unique(c(edges$parent_term, edges$child_term))
  if (!(root_term %in% all_nodes)) return(character(0))

  child_of <- split(edges$child_term, edges$parent_term)
  parent_of <- split(edges$parent_term, edges$child_term)

  descendants <- .graph_reachable(root_term, child_of)
  ancestors <- .graph_reachable(root_term, parent_of)
  unique(c(root_term, descendants, ancestors))
}

.graph_reachable <- function(start_node, adjacency) {
  visited <- character(0)
  queue <- start_node
  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]
    neighbors <- adjacency[[node]]
    if (is.null(neighbors) || length(neighbors) == 0) next
    neighbors <- unique(as.character(neighbors))
    new_nodes <- setdiff(neighbors, c(visited, start_node))
    if (length(new_nodes) > 0) {
      visited <- c(visited, new_nodes)
      queue <- c(queue, new_nodes)
    }
  }
  unique(visited)
}

.ensure_acyclic_edges <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0) return(edges)

  edges <- edges[order(-edges$score, edges$child_term, edges$parent_term), , drop = FALSE]
  keep <- rep(FALSE, nrow(edges))
  child_of <- list()

  path_exists <- function(start, target, adj) {
    if (is.null(start) || is.null(target)) return(FALSE)
    start <- as.character(start)
    target <- as.character(target)
    if (!nzchar(start) || !nzchar(target)) return(FALSE)
    if (identical(start, target)) return(TRUE)

    visited <- character(0)
    queue <- start
    while (length(queue) > 0) {
      node <- queue[[1]]
      queue <- queue[-1]
      if (identical(node, target)) return(TRUE)
      nbr <- adj[[node]]
      if (is.null(nbr) || length(nbr) == 0) next
      nbr <- unique(as.character(nbr))
      nbr <- setdiff(nbr, visited)
      if (length(nbr) == 0) next
      visited <- c(visited, nbr)
      queue <- c(queue, nbr)
    }
    FALSE
  }

  for (i in seq_len(nrow(edges))) {
    parent <- as.character(edges$parent_term[i])
    child <- as.character(edges$child_term[i])
    if (!nzchar(parent) || !nzchar(child)) next
    # Adding parent -> child would create a cycle if child can already reach parent.
    if (isTRUE(path_exists(child, parent, child_of))) next
    keep[i] <- TRUE
    existing <- child_of[[parent]]
    if (is.null(existing)) {
      child_of[[parent]] <- child
    } else {
      child_of[[parent]] <- unique(c(existing, child))
    }
  }

  out <- edges[keep, , drop = FALSE]
  row.names(out) <- NULL
  out
}

# ---- Export helpers ---------------------------------------------------------

.export_report_template_path <- function() {
  if (exists(".pkg_file", mode = "function")) {
    path <- .pkg_file("rmarkdown", "graph_export_report.Rmd")
    if (nzchar(path) && file.exists(path)) return(path)
  }
  file.path(getwd(), "inst", "rmarkdown", "graph_export_report.Rmd")
}

.export_fmt_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("NA")
  sprintf("%.1f%%", 100 * x)
}

.export_flatten_grouped_sources <- function(grouped, source_col) {
  if (is.null(grouped) || nrow(grouped) == 0 || !(source_col %in% names(grouped))) {
    return("\u2014")
  }
  vals <- as.character(grouped[[source_col]])
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (length(vals) == 0) return("\u2014")
  vals <- gsub("\\n+", "; ", vals)
  paste(vals, collapse = " | ")
}

.export_flatten_grouped_defs <- function(grouped) {
  if (is.null(grouped) || nrow(grouped) == 0 || !("definition" %in% names(grouped))) {
    return("\u2014")
  }
  defs <- as.character(grouped$definition)
  defs <- defs[!is.na(defs) & nzchar(trimws(defs))]
  if (length(defs) == 0) return("\u2014")
  defs <- gsub("\\s+", " ", defs)
  paste(defs, collapse = " || ")
}

# ---- Cache helpers ----------------------------------------------------------

.hierarchy_cache_path <- function(cache_dir) {
  file.path(cache_dir, "hierarchy_edges_cache.rds")
}

.hierarchy_cache_meta <- function(data) {
  list(
    schema = 1L,
    fingerprint = .hierarchy_fingerprint(data)
  )
}

.hierarchy_fingerprint <- function(data) {
  if (is.null(data) || nrow(data) == 0) return("empty")

  payload <- list(
    matched_term = as.character(data$matched_term),
    ipbes_defs = lapply(data$ipbes_data, function(df) {
      if (is.null(df) || !("definition" %in% names(df))) return(character(0))
      as.character(df$definition)
    }),
    ipcc_defs = lapply(data$ipcc_data, function(df) {
      if (is.null(df) || !("definition" %in% names(df))) return(character(0))
      as.character(df$definition)
    })
  )

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(payload, tf)
  unname(as.character(tools::md5sum(tf)[[1]]))
}

.load_hierarchy_cache <- function(cache_dir, expected_meta) {
  path <- .hierarchy_cache_path(cache_dir)
  if (!file.exists(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || is.null(obj$meta) || is.null(obj$edges)) return(NULL)
  if (!identical(obj$meta, expected_meta)) return(NULL)
  obj$edges
}

.packaged_hierarchy_cache_path <- function() {
  if (exists(".pkg_file", mode = "function")) {
    path <- .pkg_file("extdata", "hierarchy_edges_cache.rds")
    if (nzchar(path) && file.exists(path)) return(path)
  }
  local_path <- file.path(getwd(), "inst", "extdata", "hierarchy_edges_cache.rds")
  if (file.exists(local_path)) local_path else ""
}

.load_packaged_hierarchy_cache <- function(expected_meta) {
  path <- .packaged_hierarchy_cache_path()
  if (!nzchar(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || is.null(obj$meta) || is.null(obj$edges)) return(NULL)
  if (!identical(obj$meta, expected_meta)) return(NULL)
  obj$edges
}

.save_hierarchy_cache <- function(cache_dir, meta, edges) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  path <- .hierarchy_cache_path(cache_dir)
  tryCatch(
    saveRDS(list(meta = meta, edges = edges), path),
    error = function(e) invisible(NULL)
  )
  invisible(TRUE)
}
