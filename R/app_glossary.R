# Glossary explorer app entry point
# =============================================================================

#' Run the interactive glossary explorer app
#'
#' Launches a focused glossary browser that lets users switch between IPBES,
#' IPCC, or both sources; search terms with autocomplete; and navigate by
#' clicking highlighted glossary terms inside definitions.
#'
#' @param cache_dir Directory used to store and read cached glossary snapshots.
#'   Defaults to [tools::R_user_dir()] cache.
#' @param ... Additional arguments passed to [shiny::runApp()] (for example
#'   `launch.browser`, `port`).
#' @return Invisibly, a [shiny::shinyApp()] object.
#' @examples
#' \dontrun{
#' glossary.ipbes.ipcc::run_glossary()
#' }
#' @export
run_glossary <- function(
    cache_dir = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    ...
) {
  app <- .create_glossary_app(cache_dir = cache_dir)
  shiny::runApp(app, ...)
}

.create_glossary_app <- function(cache_dir) {
  .ensure_cache_dir(cache_dir)
  merged <- .load_merged_data(cache_dir, prepare_table_cache = FALSE)
  .register_www_assets()

  shiny::shinyApp(
    ui = .build_glossary_ui(),
    server = .build_glossary_server(merged)
  )
}

.build_glossary_ui <- function() {
  issues_url <- "https://github.com/rkrug/glossary_ipbes_ipcc/issues"
  about_url <- "custom/about_glossary.html"
  app_version <- .app_version_string()
  app_date <- paste(
    as.integer(format(Sys.Date(), "%d")),
    format(Sys.Date(), "%B"),
    format(Sys.Date(), "%Y")
  )

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "custom/custom.css"),
      shiny::tags$script(htmltools::HTML(
        "$(document).on('click', '.glossary-term-link', function(e) {
           e.preventDefault();
           var term = $(this).attr('data-term');
           if (term) {
             Shiny.setInputValue('term_click', term, {priority: 'event'});
           }
         });"
      ))
    ),
    shiny::titlePanel(title = NULL, windowTitle = "IPBES and IPCC Glossary Explorer"),
    htmltools::div(
      htmltools::h2("IPBES and IPCC Glossary Explorer", style = "margin-bottom: 0; font-size: 2.8rem;"),
      htmltools::p(
        style = "color:#555; font-size:1.45rem; font-weight:600; margin:2px 0 0 0;",
        paste0("Version ", app_version, " (", app_date, ")")
      ),
      htmltools::p(
        style = "color:#666; font-size:1.12rem; margin-top:2px;",
        shiny::actionLink(
          inputId = "about_open",
          label = "About",
          style = "display:inline; padding:0; border:none; background:none; font-size:inherit; vertical-align:baseline;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::selectizeInput(
          inputId = "term",
          label = "Term",
          choices = character(0),
          selected = NULL,
          options = list(
            placeholder = "Type to search glossary terms...",
            maxOptions = 2000,
            closeAfterSelect = TRUE,
            selectOnTab = TRUE
          )
        ),
        shiny::radioButtons(
          inputId = "source_mode",
          label = "Source",
          choices = c("IPBES" = "ipbes", "IPCC" = "ipcc", "Both" = "both"),
          selected = "both",
          inline = TRUE
        )
      )
    ),
    shiny::uiOutput("glossary_definition_view"),
    shiny::hr(),
    htmltools::div(
      class = "app-attribution",
      htmltools::div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%; gap:1rem;",
        htmltools::div(
          class = "data-links",
          style = "text-align:left;",
          "Data: ",
          htmltools::a("IPBES Glossary", href = "https://www.ipbes.net/glossary", target = "_blank"),
          " | ",
          htmltools::a("IPCC Glossary", href = "https://apps.ipcc.ch/glossary/search.php", target = "_blank"),
          htmltools::tags$br(),
          "Issues: ",
          htmltools::a("GitHub Issues", href = issues_url, target = "_blank")
        ),
        htmltools::div(
          class = "copyright-note",
          style = "text-align:right;",
          "Developed by ",
          htmltools::a(
            "Rainer M Krug",
            href = "mailto:Rainer.Krug@SIB.swiss,Rainer.Krug@senckenberg.de",
            target = "_blank"
          ),
          " - ",
          htmltools::a(
            "SIB Swiss Institute of Bioinformatics",
            href = "https://www.sib.swiss/",
            target = "_blank"
          ),
          " and ",
          htmltools::a(
            "Senckenberg Biodiversity and Climate",
            href = "https://www.senckenberg.de/en/",
            target = "_blank"
          )
        )
      )
    )
  )
}

.build_glossary_server <- function(initial_data) {
  function(input, output, session) {
    about_url <- "custom/about_glossary.html"
    merged_rv <- shiny::reactiveVal(initial_data)
    active_term_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$about_open, {
      about_src <- paste0(about_url, "?t=", as.integer(Sys.time()))
      shiny::showModal(
        shiny::modalDialog(
          title = "About",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          htmltools::tags$iframe(
            src = about_src,
            style = "width:100%; height:62vh; border:0; border-radius:6px;"
          )
        )
      )
    }, ignoreInit = TRUE)

    mode_r <- shiny::reactive({
      mode <- tolower(trimws(as.character(input$source_mode)))
      if (!nzchar(mode) || is.na(mode) || !mode %in% c("ipbes", "ipcc", "both")) {
        "both"
      } else {
        mode
      }
    })

    terms_r <- shiny::reactive({
      .glossary_term_catalog(merged_rv(), mode_r())
    })

    shiny::observeEvent(terms_r(), {
      choices <- terms_r()
      selected <- active_term_rv()
      selected <- .glossary_resolve_choice(selected, choices)
      has_selected <- nzchar(selected)
      active_term_rv(if (has_selected) selected else NULL)
      shiny::updateSelectizeInput(
        session,
        inputId = "term",
        choices = choices,
        selected = if (has_selected) selected else "",
        server = TRUE
      )
      if (!has_selected) {
        session$sendInputMessage("term", list(value = ""))
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$term, {
      choices <- terms_r()
      selected <- .glossary_resolve_choice(input$term, choices)
      if (nzchar(selected)) {
        active_term_rv(selected)
      } else {
        active_term_rv(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$term_click, {
      choices <- terms_r()
      clicked <- .glossary_resolve_choice(input$term_click, choices)
      if (nzchar(clicked)) {
        active_term_rv(clicked)
        shiny::updateSelectizeInput(
          session,
          inputId = "term",
          choices = choices,
          selected = clicked,
          server = TRUE
        )
      }
    }, ignoreInit = TRUE)

    selected_term_r <- shiny::reactive({
      selected <- .glossary_resolve_choice(active_term_rv(), terms_r())
      choices <- terms_r()
      if (nzchar(selected) && selected %in% choices) {
        return(selected)
      }
      ""
    })

    dict_r <- shiny::reactive({
      .glossary_highlight_dictionary(terms_r())
    })

    hover_lookup_r <- shiny::reactive({
      .glossary_hover_lookup(merged_rv(), mode_r())
    })

    selected_row_r <- shiny::reactive({
      .glossary_find_row(merged_rv(), selected_term_r(), mode_r())
    })

    output$glossary_definition_view <- shiny::renderUI({
      row <- selected_row_r()
      if (is.null(row) || nrow(row) == 0) {
        return(NULL)
      }

      mode <- mode_r()
      dict <- dict_r()
      hover_lookup <- hover_lookup_r()
      see_also_terms <- .glossary_collect_link_terms(row, mode, dict)
      see_also_ui <- .glossary_see_also_ui(see_also_terms, mode)

      if (identical(mode, "ipbes")) {
        return(
          htmltools::div(
            class = "glossary-sections",
            .glossary_source_section_ui(
              row = row,
              source = "ipbes",
              dict = dict,
              hover_lookup = hover_lookup,
              term_label = selected_term_r()
            ),
            see_also_ui
          )
        )
      }

      if (identical(mode, "ipcc")) {
        return(
          htmltools::div(
            class = "glossary-sections",
            .glossary_source_section_ui(
              row = row,
              source = "ipcc",
              dict = dict,
              hover_lookup = hover_lookup,
              term_label = selected_term_r()
            ),
            see_also_ui
          )
        )
      }

      htmltools::div(
        class = "glossary-sections",
        .glossary_source_section_ui(
          row = row,
          source = "ipbes",
          dict = dict,
          hover_lookup = hover_lookup,
          term_label = selected_term_r()
        ),
        .glossary_source_section_ui(
          row = row,
          source = "ipcc",
          dict = dict,
          hover_lookup = hover_lookup,
          term_label = selected_term_r()
        ),
        see_also_ui
      )
    })
  }
}

.glossary_term_catalog <- function(data, mode = "both") {
  if (is.null(data) || nrow(data) == 0) return(character(0))

  terms <- switch(mode,
    ipbes = if ("ipbes_concept" %in% names(data)) data$ipbes_concept else character(0),
    ipcc = if ("ipcc_term" %in% names(data)) data$ipcc_term else character(0),
    both = c(
      if ("matched_term" %in% names(data)) data$matched_term else character(0),
      if ("ipbes_concept" %in% names(data)) data$ipbes_concept else character(0),
      if ("ipcc_term" %in% names(data)) data$ipcc_term else character(0)
    ),
    character(0)
  )

  terms <- as.character(terms)
  terms <- terms[!is.na(terms)]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) return(character(0))

  # De-duplicate case-insensitively so variants like "Biodiversity" and
  # "biodiversity" appear only once in the selector.
  key <- tolower(terms)
  terms <- terms[!duplicated(key)]
  terms[order(tolower(terms), terms)]
}

.glossary_find_row <- function(data, term, mode = "both") {
  if (is.null(data) || nrow(data) == 0 || is.null(term) || !nzchar(term)) return(NULL)

  term <- trimws(as.character(term))
  norm_term <- normalise_term(term)
  idx <- integer(0)

  if (identical(mode, "ipbes") && "ipbes_concept" %in% names(data)) {
    idx <- which(data$ipbes_concept == term)
    if (length(idx) == 0) idx <- which(normalise_term(data$ipbes_concept) == norm_term)
  } else if (identical(mode, "ipcc") && "ipcc_term" %in% names(data)) {
    idx <- which(data$ipcc_term == term)
    if (length(idx) == 0) idx <- which(normalise_term(data$ipcc_term) == norm_term)
  } else {
    if ("matched_term" %in% names(data)) {
      idx <- which(data$matched_term == term)
      if (length(idx) == 0) idx <- which(normalise_term(data$matched_term) == norm_term)
    }
    if (length(idx) == 0 && "ipbes_concept" %in% names(data)) {
      idx <- which(data$ipbes_concept == term)
      if (length(idx) == 0) idx <- which(normalise_term(data$ipbes_concept) == norm_term)
    }
    if (length(idx) == 0 && "ipcc_term" %in% names(data)) {
      idx <- which(data$ipcc_term == term)
      if (length(idx) == 0) idx <- which(normalise_term(data$ipcc_term) == norm_term)
    }
  }

  if (length(idx) == 0) return(NULL)
  data[idx[[1]], , drop = FALSE]
}

.glossary_resolve_choice <- function(term, choices) {
  if (is.null(term) || length(term) == 0 || is.null(choices) || length(choices) == 0) {
    return("")
  }

  term <- trimws(as.character(term[[1]]))
  if (is.na(term) || !nzchar(term)) return("")

  choices <- as.character(choices)
  choices <- choices[!is.na(choices)]
  if (length(choices) == 0) return("")

  exact_idx <- which(choices == term)
  if (length(exact_idx) > 0) return(choices[[exact_idx[[1]]]])

  norm_term <- normalise_term(term)
  norm_choices <- normalise_term(choices)
  idx <- which(norm_choices == norm_term)
  if (length(idx) > 0) return(choices[[idx[[1]]]])

  ""
}

.glossary_source_section_ui <- function(row, source, dict, hover_lookup, term_label = "") {
  title_term <- trimws(as.character(term_label))
  if (!nzchar(title_term) && "matched_term" %in% names(row)) {
    title_term <- trimws(as.character(row$matched_term[[1]]))
  }
  if (!nzchar(title_term)) title_term <- "Term"

  if (identical(source, "ipbes")) {
    detail_df <- if ("ipbes_data" %in% names(row) && length(row$ipbes_data) > 0) {
      row$ipbes_data[[1]]
    } else {
      data.frame(assessment = character(), definition = character(), stringsAsFactors = FALSE)
    }
    grouped <- .glossary_group_definitions(detail_df, source_col = "assessment")
    return(.glossary_definition_section_ui(
      title = .glossary_section_header_title(title_term, "IPBES Glossary"),
      source_label = "Assessments",
      source_class = "ipbes",
      grouped = grouped,
      dict = dict,
      hover_lookup = hover_lookup
    ))
  }

  detail_df <- if ("ipcc_data" %in% names(row) && length(row$ipcc_data) > 0) {
    row$ipcc_data[[1]]
  } else {
    data.frame(report = character(), definition = character(), stringsAsFactors = FALSE)
  }
  grouped <- .glossary_group_definitions(detail_df, source_col = "report")
  .glossary_definition_section_ui(
    title = .glossary_section_header_title(title_term, "IPCC Glossary"),
    source_label = "Reports",
    source_class = "ipcc",
    grouped = grouped,
    dict = dict,
    hover_lookup = hover_lookup
  )
}

.glossary_section_header_title <- function(term, glossary_name) {
  is_ipbes <- grepl("IPBES", glossary_name, fixed = TRUE)
  term_style <- if (is_ipbes) {
    paste(
      "color:#245b3f;",
      "font-family:'SFMono-Regular',Consolas,'Liberation Mono',Menlo,monospace;",
      "font-size:0.9em; font-weight:700;"
    )
  } else {
    paste(
      "color:#1f4f84;",
      "font-family:'SFMono-Regular',Consolas,'Liberation Mono',Menlo,monospace;",
      "font-size:0.9em; font-weight:700;"
    )
  }

  htmltools::tagList(
    htmltools::tags$span(class = "glossary-header-term", style = term_style, term),
    htmltools::tags$span(class = "glossary-header-suffix", style = "font-weight:700; color:#334155;", paste0(" in ", glossary_name))
  )
}

.glossary_definition_section_ui <- function(
    title,
    source_label,
    source_class,
    grouped,
    dict,
    hover_lookup
) {
  section_class <- paste("glossary-source-section", paste0("glossary-source-", source_class))
  section_style <- if (identical(source_class, "ipbes")) {
    "background:#f2fbf3; border-color:#d7efd9;"
  } else if (identical(source_class, "ipcc")) {
    "background:#f2f8ff; border-color:#d6e7fb;"
  } else {
    NULL
  }
  card_style <- if (identical(source_class, "ipbes")) {
    "background:#e4f3e5; border-color:#c7e4ca;"
  } else if (identical(source_class, "ipcc")) {
    "background:#e3efff; border-color:#c7dcf6;"
  } else {
    NULL
  }

  if (is.null(grouped) || nrow(grouped) == 0) {
    return(htmltools::div(
      class = section_class,
      style = section_style,
      htmltools::h4(title),
      htmltools::div(class = "glossary-empty", "No definitions available.")
    ))
  }

  source_col <- setdiff(names(grouped), "definition")[[1]]
  cards <- lapply(seq_len(nrow(grouped)), function(i) {
    src <- as.character(grouped[[source_col]][i])
    src <- .glossary_source_inline(src)
    def <- as.character(grouped$definition[i])
    def_html <- .glossary_highlight_definition(def, dict, hover_lookup)

    htmltools::div(
      class = "glossary-def-card",
      style = card_style,
        htmltools::div(
          class = "glossary-def-body",
          style = "font-size:1.36rem; line-height:1.65;",
          htmltools::HTML(paste0("&ldquo;", def_html, "&rdquo;"))
        ),
        htmltools::div(
          class = "glossary-def-meta",
          style = "display:block; margin-top:1.1em; padding-top:0.1em; font-size:1.06rem; line-height:1.4; color:#556176;",
          htmltools::tags$strong(style = "font-size:inherit; font-weight:700;", "As defined in: "),
          src
        )
    )
  })

  htmltools::div(
    class = section_class,
    style = section_style,
    htmltools::h4(title),
    cards
  )
}

.glossary_collect_link_terms <- function(row, mode, dict) {
  if (is.null(row) || nrow(row) == 0 || is.null(dict) || length(dict$terms) == 0) {
    return(list(ipbes = character(0), ipcc = character(0), all = character(0)))
  }

  add_defs <- function(detail_df) {
    if (is.null(detail_df) || nrow(detail_df) == 0 || !("definition" %in% names(detail_df))) return(character(0))
    v <- trimws(as.character(detail_df$definition))
    v[!is.na(v) & nzchar(v)]
  }

  collect_from_df <- function(detail_df) {
    defs <- add_defs(detail_df)
    if (length(defs) == 0) return(character(0))
    terms <- unlist(lapply(defs, .glossary_find_terms_in_text, dict = dict), use.names = FALSE)
    if (length(terms) == 0) return(character(0))
    terms[!duplicated(terms)]
  }

  ipbes_terms <- character(0)
  ipcc_terms <- character(0)

  if (identical(mode, "ipbes") || identical(mode, "both")) {
    ipbes_df <- if ("ipbes_data" %in% names(row) && length(row$ipbes_data) > 0) row$ipbes_data[[1]] else NULL
    ipbes_terms <- collect_from_df(ipbes_df)
  }
  if (identical(mode, "ipcc") || identical(mode, "both")) {
    ipcc_df <- if ("ipcc_data" %in% names(row) && length(row$ipcc_data) > 0) row$ipcc_data[[1]] else NULL
    ipcc_terms <- collect_from_df(ipcc_df)
  }

  all_terms <- c(ipbes_terms, ipcc_terms)
  if (length(all_terms) > 0) all_terms <- all_terms[!duplicated(all_terms)]

  list(
    ipbes = ipbes_terms,
    ipcc = ipcc_terms,
    all = all_terms
  )
}

.glossary_find_terms_in_text <- function(text, dict) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(character(0))
  if (is.null(dict) || length(dict$terms) == 0 || length(dict$patterns) == 0) return(character(0))

  txt <- as.character(text)
  n <- nchar(txt)
  if (n < 1L) return(character(0))

  occupied <- rep(FALSE, n)
  found <- list()

  for (i in seq_along(dict$terms)) {
    term <- dict$terms[[i]]
    pattern <- dict$patterns[[i]]
    mm <- gregexpr(pattern, txt, perl = TRUE, ignore.case = TRUE)[[1]]
    if (length(mm) == 1 && mm[[1]] == -1) next
    ll <- attr(mm, "match.length")
    for (j in seq_along(mm)) {
      start <- as.integer(mm[[j]])
      mlen <- as.integer(ll[[j]])
      end <- start + mlen - 1L
      if (start < 1L || end < start || end > n) next
      if (any(occupied[start:end])) next
      occupied[start:end] <- TRUE
      found[[length(found) + 1L]] <- list(start = start, term = term)
    }
  }

  if (length(found) == 0) return(character(0))
  found <- found[order(vapply(found, function(x) x$start, integer(1)))]
  terms <- vapply(found, function(x) as.character(x$term), character(1))
  terms[!duplicated(terms)]
}

.glossary_see_also_ui <- function(terms, mode = "both") {
  if (is.null(terms) || length(terms$all) == 0) {
    return(htmltools::div(
      class = "glossary-source-section glossary-see-also",
      htmltools::h4("See also"),
      htmltools::div(class = "glossary-empty", "No linked glossary terms in the definitions shown above.")
    ))
  }

  sorted_terms <- sort(as.character(terms$all), na.last = TRUE)
  all_links <- lapply(seq_along(sorted_terms), function(i) {
    term <- sorted_terms[[i]]
    htmltools::tagList(
      htmltools::tags$a(
        href = "#",
        class = "glossary-term-link glossary-see-link",
        `data-term` = term,
        term
      ),
      if (i < length(sorted_terms)) htmltools::HTML(", ") else NULL
    )
  })

  see_content <- htmltools::div(class = "glossary-see-links", all_links)

  htmltools::div(
    class = "glossary-source-section glossary-see-also",
    htmltools::h4("See also"),
    see_content
  )
}

.glossary_group_definitions <- function(detail_df, source_col) {
  .empty_grouped <- function() {
    out <- data.frame(character(0), character(0), stringsAsFactors = FALSE)
    names(out) <- c(source_col, "definition")
    out
  }

  if (is.null(detail_df) || nrow(detail_df) == 0 || !("definition" %in% names(detail_df))) {
    return(.empty_grouped())
  }

  defs <- trimws(as.character(detail_df$definition))
  keep <- !is.na(defs) & nzchar(defs)
  if (!any(keep)) return(.empty_grouped())

  detail_df <- detail_df[keep, , drop = FALSE]
  defs <- trimws(as.character(detail_df$definition))
  src <- if (source_col %in% names(detail_df)) trimws(as.character(detail_df[[source_col]])) else rep("", length(defs))

  rows <- list()
  for (i in seq_along(defs)) {
    def_i <- defs[[i]]
    src_i <- src[[i]]
    idx <- match(def_i, vapply(rows, function(x) x$definition, character(1)))
    if (is.na(idx)) {
      rows[[length(rows) + 1]] <- list(
        definition = def_i,
        sources = if (!is.na(src_i) && nzchar(src_i)) src_i else character(0)
      )
    } else if (!is.na(src_i) && nzchar(src_i) && !src_i %in% rows[[idx]]$sources) {
      rows[[idx]]$sources <- c(rows[[idx]]$sources, src_i)
    }
  }

  out <- data.frame(
    sources = vapply(rows, function(x) paste(x$sources, collapse = "\n"), character(1)),
    definition = vapply(rows, function(x) x$definition, character(1)),
    stringsAsFactors = FALSE
  )
  names(out)[1] <- source_col
  out
}

.glossary_source_inline <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return("\u2014")
  parts <- strsplit(as.character(value), "\n", fixed = TRUE)[[1]]
  parts <- parts[nzchar(trimws(parts))]
  if (length(parts) == 0) return("\u2014")
  parts <- vapply(parts, .expand_ipcc_report_name, character(1), USE.NAMES = FALSE)
  paste(parts, collapse = "; ")
}

.glossary_hover_lookup <- function(data, mode = "both") {
  terms <- .glossary_term_catalog(data, mode)
  if (length(terms) == 0) return(character(0))

  out <- stats::setNames(rep("No definition available.", length(terms)), terms)
  for (term in terms) {
    row <- .glossary_find_row(data, term, mode)
    if (is.null(row) || nrow(row) == 0) next

    if (identical(mode, "ipbes")) {
      out[[term]] <- .glossary_hover_text_from_row(row, "ipbes")
      next
    }
    if (identical(mode, "ipcc")) {
      out[[term]] <- .glossary_hover_text_from_row(row, "ipcc")
      next
    }

    ipbes_txt <- .glossary_hover_text_from_row(row, "ipbes")
    ipcc_txt <- .glossary_hover_text_from_row(row, "ipcc")
    out[[term]] <- paste(ipbes_txt, ipcc_txt, sep = "\n")
  }

  out
}

.glossary_hover_text_from_row <- function(row, source) {
  if (identical(source, "ipbes")) {
    detail_df <- if ("ipbes_data" %in% names(row) && length(row$ipbes_data) > 0) {
      row$ipbes_data[[1]]
    } else {
      data.frame(assessment = character(), definition = character(), stringsAsFactors = FALSE)
    }
    grouped <- .glossary_group_definitions(detail_df, "assessment")
    if (nrow(grouped) == 0) return("IPBES: no definition available.")
    lines <- vapply(seq_len(min(4L, nrow(grouped))), function(i) {
      src <- .glossary_source_inline(grouped$assessment[i])
      def <- trimws(gsub("\\s+", " ", as.character(grouped$definition[i])))
      if (nchar(def) > 180) def <- paste0(substr(def, 1, 177), "...")
      paste0("IPBES - ", src, ": ", def)
    }, character(1))
    if (nrow(grouped) > 4L) lines <- c(lines, "...")
    return(paste(lines, collapse = "\n"))
  }

  detail_df <- if ("ipcc_data" %in% names(row) && length(row$ipcc_data) > 0) {
    row$ipcc_data[[1]]
  } else {
    data.frame(report = character(), definition = character(), stringsAsFactors = FALSE)
  }
  grouped <- .glossary_group_definitions(detail_df, "report")
  if (nrow(grouped) == 0) return("IPCC: no definition available.")
  lines <- vapply(seq_len(min(4L, nrow(grouped))), function(i) {
    src <- .glossary_source_inline(grouped$report[i])
    def <- trimws(gsub("\\s+", " ", as.character(grouped$definition[i])))
    if (nchar(def) > 180) def <- paste0(substr(def, 1, 177), "...")
    paste0("IPCC - ", src, ": ", def)
  }, character(1))
  if (nrow(grouped) > 4L) lines <- c(lines, "...")
  paste(lines, collapse = "\n")
}

.glossary_highlight_dictionary <- function(terms) {
  terms <- as.character(terms)
  terms <- terms[!is.na(terms)]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]
  terms <- unique(terms)

  if (length(terms) == 0) {
    return(list(terms = character(0), patterns = character(0)))
  }

  terms <- terms[order(-nchar(terms), tolower(terms), terms)]
  escaped <- vapply(terms, .glossary_escape_regex, character(1), USE.NAMES = FALSE)
  escaped <- gsub("\\s+", "\\\\s+", escaped)
  patterns <- paste0("(?<![[:alnum:]])", escaped, "(?![[:alnum:]])")

  list(
    terms = terms,
    patterns = patterns
  )
}

.glossary_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\.?-])", "\\\\\\1", as.character(x), perl = TRUE)
}

.glossary_highlight_definition <- function(text, dict, hover_lookup) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return("\u2014")
  if (is.null(dict) || length(dict$terms) == 0 || length(dict$patterns) == 0) {
    return(as.character(htmltools::htmlEscape(text)))
  }
  txt <- as.character(text)
  n <- nchar(txt)
  occupied <- rep(FALSE, n)
  found <- list()

  for (i in seq_along(dict$terms)) {
    term <- dict$terms[[i]]
    pattern <- dict$patterns[[i]]
    mm <- gregexpr(pattern, txt, perl = TRUE, ignore.case = TRUE)[[1]]
    if (length(mm) == 1 && mm[[1]] == -1) next
    ll <- attr(mm, "match.length")
    for (j in seq_along(mm)) {
      start <- as.integer(mm[[j]])
      mlen <- as.integer(ll[[j]])
      end <- start + mlen - 1L
      if (start < 1L || end < start || end > n) next
      if (any(occupied[start:end])) next
      occupied[start:end] <- TRUE
      found[[length(found) + 1L]] <- list(
        start = start,
        end = end,
        term = term
      )
    }
  }

  if (length(found) == 0) {
    return(as.character(htmltools::htmlEscape(txt)))
  }
  found <- found[order(vapply(found, function(x) x$start, integer(1)))]

  cursor <- 1L
  parts <- character(0)
  for (m in found) {
    start <- m$start
    end <- m$end
    term <- as.character(m$term)

    if (start > cursor) {
      parts <- c(parts, as.character(htmltools::htmlEscape(substr(txt, cursor, start - 1L))))
    }

    matched <- substr(txt, start, end)
    tooltip <- hover_lookup[[term]]
    if (is.null(tooltip) || !nzchar(tooltip)) tooltip <- "No definition available."
    tooltip_attr <- as.character(htmltools::htmlEscape(tooltip))
    tooltip_attr <- gsub("\n", "&#10;", tooltip_attr, fixed = TRUE)

    parts <- c(parts, paste0(
      "<a href=\"#\" class=\"glossary-term-link\" data-term=\"",
      as.character(htmltools::htmlEscape(term)),
      "\" title=\"",
      tooltip_attr,
      "\">",
      as.character(htmltools::htmlEscape(matched)),
      "</a>"
    ))

    cursor <- end + 1L
  }

  if (cursor <= n) {
    parts <- c(parts, as.character(htmltools::htmlEscape(substr(txt, cursor, n))))
  }
  paste(parts, collapse = "")
}
