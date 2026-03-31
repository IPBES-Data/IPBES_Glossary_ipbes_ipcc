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
  multilingual <- load_unified_multilingual_runtime(
    path = file.path(cache_dir, "glossary_multilingual_runtime.rds")
  )
  if (is.null(multilingual) || nrow(multilingual) == 0) {
    multilingual <- load_unified_multilingual_runtime()
  }
  .register_www_assets()

  shiny::shinyApp(
    ui = .build_glossary_ui(),
    server = .build_glossary_server(merged, multilingual)
  )
}

.build_glossary_ui <- function() {
  issues_url <- "https://github.com/rkrug/glossary_ipbes_ipcc/issues"
  about_url <- "custom/about_glossary.html"
  app_version <- "1.9 beta"
  css_href <- paste0("custom/custom.css?v=", as.integer(Sys.time()))
  app_date <- paste(
    as.integer(format(Sys.Date(), "%d")),
    format(Sys.Date(), "%B"),
    format(Sys.Date(), "%Y")
  )

    shiny::fluidPage(
      shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = css_href),
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
        shiny::selectInput(
          inputId = "main_language",
          label = "Main language",
          choices = c(
            "English (en)" = "en",
            "Arabic (ar)" = "ar",
            "Spanish (es)" = "es",
            "French (fr)" = "fr",
            "Russian (ru)" = "ru",
            "Chinese (zh)" = "zh"
          ),
          selected = "en"
        ),
        shiny::radioButtons(
          inputId = "source_mode",
          label = "Source",
          choices = c("IPBES" = "ipbes", "IPCC" = "ipcc", "Both" = "both"),
          selected = "both",
          inline = TRUE
        ),
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

.build_glossary_server <- function(initial_data, multilingual_data = NULL) {
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

    languages_r <- shiny::reactive({
      .glossary_languages()
    })

    main_language_r <- shiny::reactive({
      lang <- tolower(trimws(as.character(input$main_language)))
      valid <- languages_r()
      if (!nzchar(lang) || is.na(lang) || !lang %in% valid) "en" else lang
    })

    multilingual_r <- shiny::reactive({
      .glossary_prepare_multilingual(multilingual_data, merged_rv())
    })

    catalog_r <- shiny::reactive({
      .glossary_build_catalog(
        multilingual = multilingual_r(),
        mode = mode_r(),
        main_language = main_language_r()
      )
    })

    choices_r <- shiny::reactive({
      catalog <- catalog_r()
      .glossary_catalog_choices(catalog)
    })

    shiny::observeEvent(catalog_r(), {
      catalog <- catalog_r()
      choices <- choices_r()
      selected <- active_term_rv()
      selected <- .glossary_resolve_key(selected, catalog$value)
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
      selected <- .glossary_resolve_key(input$term, catalog_r()$value)
      if (nzchar(selected)) {
        active_term_rv(selected)
      } else {
        active_term_rv(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$term_click, {
      choices <- choices_r()
      clicked <- .glossary_resolve_key(input$term_click, catalog_r()$value)
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

    selected_key_r <- shiny::reactive({
      selected <- .glossary_resolve_key(active_term_rv(), catalog_r()$value)
      if (nzchar(selected)) selected else ""
    })

    selected_family_r <- shiny::reactive({
      .glossary_selected_family(
        selected_key = selected_key_r(),
        catalog = catalog_r(),
        multilingual = multilingual_r(),
        main_language = main_language_r(),
        mode = mode_r()
      )
    })

    dict_r <- shiny::reactive({
      .glossary_highlight_dictionary_map(
        multilingual = multilingual_r(),
        catalog = catalog_r(),
        mode = mode_r()
      )
    })

    output$glossary_definition_view <- shiny::renderUI({
      selected_key <- selected_key_r()
      if (!nzchar(selected_key)) {
        return(NULL)
      }

      mode <- mode_r()
      main_language <- main_language_r()
      family <- selected_family_r()
      selected_catalog <- .glossary_catalog_row(catalog_r(), selected_key)
      if (is.null(selected_catalog) || nrow(selected_catalog) == 0) return(NULL)

      dict <- dict_r()
      hover_keys <- c(
        selected_key,
        if (!is.na(family$ipbes_concept_id) && nzchar(family$ipbes_concept_id)) paste0("IPBES::", family$ipbes_concept_id) else character(0),
        if (!is.na(family$ipcc_concept_id) && nzchar(family$ipcc_concept_id)) paste0("IPCC::", family$ipcc_concept_id) else character(0)
      )
      hover_lookup <- .glossary_hover_lookup_subset(
        multilingual = multilingual_r(),
        keys = hover_keys
      )
      ipbes_section <- .glossary_multilingual_source_section_ui(
        multilingual = multilingual_r(),
        source = "IPBES",
        concept_id = family$ipbes_concept_id,
        languages = languages_r(),
        dict = dict,
        hover_lookup = hover_lookup,
        main_language = main_language
      )
      ipcc_section <- .glossary_multilingual_source_section_ui(
        multilingual = multilingual_r(),
        source = "IPCC",
        concept_id = family$ipcc_concept_id,
        languages = languages_r(),
        dict = dict,
        hover_lookup = hover_lookup,
        main_language = main_language
      )
      see_also_terms <- .glossary_collect_link_keys(
        source_sections = list(ipbes = ipbes_section$data, ipcc = ipcc_section$data),
        mode = mode,
        dict = dict
      )
      see_also_ui <- .glossary_see_also_ui_multilingual(
        links = see_also_terms,
        catalog = catalog_r(),
        multilingual = multilingual_r(),
        main_language = main_language
      )

      sections <- if (identical(mode, "ipbes")) {
        list(ipbes_section$ui, see_also_ui)
      } else if (identical(mode, "ipcc")) {
        list(ipcc_section$ui, see_also_ui)
      } else {
        list(ipbes_section$ui, ipcc_section$ui, see_also_ui)
      }

      do.call(htmltools::div, c(list(
        class = "glossary-sections",
        `data-selected-term` = selected_catalog$label[[1]]
      ), sections))
    })
  }
}

.glossary_languages <- function() {
  c("en", "ar", "es", "fr", "ru", "zh")
}

.glossary_prepare_multilingual <- function(multilingual, merged) {
  if (!is.null(multilingual) && nrow(multilingual) > 0) {
    out <- as.data.frame(multilingual, stringsAsFactors = FALSE)
    if (!("source" %in% names(out))) out$source <- NA_character_
    out$source <- toupper(trimws(as.character(out$source)))
    out <- out[out$source %in% c("IPBES", "IPCC"), , drop = FALSE]
    return(out)
  }
  .glossary_multilingual_from_merged(merged)
}

.glossary_multilingual_from_merged <- function(merged) {
  if (is.null(merged) || nrow(merged) == 0) return(data.frame())
  rows <- list()
  add_row <- function(source, concept_id, report, term, definition) {
    rows[[length(rows) + 1L]] <<- data.frame(
      source = source,
      concept_id = concept_id,
      report_or_assessment = report,
      language = "en",
      term = term,
      definition = definition,
      alt_labels = NA_character_,
      is_available = TRUE,
      is_fetched = TRUE,
      fetch_status = "fallback_from_merged",
      source_endpoint = "merged_cache",
      downloaded_at = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(merged))) {
    if (!is.na(merged$ipbes_concept[[i]]) && nzchar(trimws(merged$ipbes_concept[[i]]))) {
      cid <- paste0("ipbes:fallback:", normalise_term(merged$ipbes_concept[[i]]))
      ipbes_df <- merged$ipbes_data[[i]]
      if (!is.null(ipbes_df) && nrow(ipbes_df) > 0) {
        for (j in seq_len(nrow(ipbes_df))) {
          add_row(
            "IPBES",
            cid,
            trimws(as.character(ipbes_df$assessment[[j]])),
            trimws(as.character(merged$ipbes_concept[[i]])),
            trimws(as.character(ipbes_df$definition[[j]]))
          )
        }
      }
    }
    if (!is.na(merged$ipcc_term[[i]]) && nzchar(trimws(merged$ipcc_term[[i]]))) {
      cid <- paste0("ipcc:fallback:", normalise_term(merged$ipcc_term[[i]]))
      ipcc_df <- merged$ipcc_data[[i]]
      if (!is.null(ipcc_df) && nrow(ipcc_df) > 0) {
        for (j in seq_len(nrow(ipcc_df))) {
          add_row(
            "IPCC",
            cid,
            trimws(as.character(ipcc_df$report[[j]])),
            trimws(as.character(merged$ipcc_term[[i]])),
            trimws(as.character(ipcc_df$definition[[j]]))
          )
        }
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

.glossary_build_catalog <- function(multilingual, mode = "both", main_language = "en") {
  if (is.null(multilingual) || nrow(multilingual) == 0) {
    return(data.frame(value = character(), label = character(), source = character(), concept_id = character(), english_key = character(), stringsAsFactors = FALSE))
  }
  df <- multilingual
  df$source <- toupper(trimws(as.character(df$source)))
  df$language <- tolower(trimws(as.character(df$language)))
  df$term <- trimws(as.character(df$term))
  df$is_available <- as.logical(df$is_available)

  keep_sources <- switch(mode,
    ipbes = "IPBES",
    ipcc = "IPCC",
    both = c("IPBES", "IPCC"),
    c("IPBES", "IPCC")
  )
  df <- df[df$source %in% keep_sources, , drop = FALSE]
  if (nrow(df) == 0) {
    return(data.frame(value = character(), label = character(), source = character(), concept_id = character(), english_key = character(), stringsAsFactors = FALSE))
  }

  key <- paste(df$source, df$concept_id, sep = "::")
  keys <- unique(key)
  out <- vector("list", length(keys))
  for (i in seq_along(keys)) {
    k <- keys[[i]]
    parts <- strsplit(k, "::", fixed = TRUE)[[1]]
    src <- parts[[1]]
    cid <- parts[[2]]
    sub <- df[key == k, , drop = FALSE]
    sub$term <- trimws(as.character(sub$term))
    sub$term[!nzchar(sub$term)] <- NA_character_
    avail <- as.logical(sub$is_available)
    main_rows <- sub[
      sub$language == main_language &
        !is.na(avail) &
        avail &
        !is.na(sub$term) &
        nzchar(sub$term),
      ,
      drop = FALSE
    ]
    if (nrow(main_rows) == 0) next
    en_rows <- sub[
      sub$language == "en" &
        !is.na(avail) &
        avail &
        !is.na(sub$term) &
        nzchar(sub$term),
      ,
      drop = FALSE
    ]
    en_term <- if (nrow(en_rows) > 0) en_rows$term[[1]] else main_rows$term[[1]]
    out[[i]] <- data.frame(
      value = k,
      label = main_rows$term[[1]],
      source = src,
      concept_id = cid,
      english_key = normalise_term(en_term),
      stringsAsFactors = FALSE
    )
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    return(data.frame(value = character(), label = character(), source = character(), concept_id = character(), english_key = character(), stringsAsFactors = FALSE))
  }
  catalog <- do.call(rbind, out)

  # Keep each term only once per source in the selector.
  # IPBES can have multiple concept/report entries with the same visible term.
  src_term_key <- paste(catalog$source, normalise_term(catalog$label), sep = "::")
  keep_idx <- !duplicated(src_term_key)
  catalog <- catalog[keep_idx, , drop = FALSE]

  # If the same visible term exists across sources, annotate with source label.
  dup <- duplicated(normalise_term(catalog$label)) | duplicated(normalise_term(catalog$label), fromLast = TRUE)
  if (any(dup)) {
    catalog$label[dup] <- paste0(catalog$label[dup], " [", catalog$source[dup], "]")
  }
  ord <- order(tolower(catalog$label), catalog$label)
  catalog[ord, , drop = FALSE]
}

.glossary_catalog_choices <- function(catalog) {
  if (is.null(catalog) || nrow(catalog) == 0) return(character(0))
  values <- trimws(as.character(catalog$value))
  labels <- trimws(as.character(catalog$label))
  keep <- !is.na(values) & nzchar(values) & !is.na(labels) & nzchar(labels)
  if (!any(keep)) return(character(0))
  stats::setNames(values[keep], labels[keep])
}

.glossary_resolve_key <- function(value, valid_values) {
  if (is.null(value) || length(value) == 0 || is.null(valid_values) || length(valid_values) == 0) return("")
  value <- trimws(as.character(value[[1]]))
  if (!nzchar(value)) return("")
  valid_values <- as.character(valid_values)
  if (value %in% valid_values) value else ""
}

.glossary_catalog_row <- function(catalog, key) {
  if (is.null(catalog) || nrow(catalog) == 0 || !nzchar(key)) return(NULL)
  idx <- which(catalog$value == key)
  if (length(idx) == 0) return(NULL)
  catalog[idx[[1]], , drop = FALSE]
}

.glossary_selected_family <- function(selected_key, catalog, multilingual, main_language, mode = "both") {
  out <- list(ipbes_concept_id = NA_character_, ipcc_concept_id = NA_character_)
  if (!nzchar(selected_key)) return(out)
  row <- .glossary_catalog_row(catalog, selected_key)
  if (is.null(row) || nrow(row) == 0) return(out)

  src <- row$source[[1]]
  cid <- row$concept_id[[1]]
  if (src == "IPBES") out$ipbes_concept_id <- cid
  if (src == "IPCC") out$ipcc_concept_id <- cid

  if (!identical(mode, "both")) return(out)

  en_key <- row$english_key[[1]]
  if (!nzchar(en_key) || is.null(catalog) || nrow(catalog) == 0) return(out)

  ipbes_idx <- which(catalog$source == "IPBES" & catalog$english_key == en_key)
  ipcc_idx <- which(catalog$source == "IPCC" & catalog$english_key == en_key)
  if (length(ipbes_idx) > 0) out$ipbes_concept_id <- as.character(catalog$concept_id[[ipbes_idx[[1]]]])
  if (length(ipcc_idx) > 0) out$ipcc_concept_id <- as.character(catalog$concept_id[[ipcc_idx[[1]]]])
  out
}

.glossary_multilingual_source_section_ui <- function(
    multilingual,
    source,
    concept_id,
    languages,
    dict,
    hover_lookup,
    main_language
) {
  source <- toupper(trimws(as.character(source)))
  source_title <- if (identical(source, "IPBES")) "IPBES Glossary" else "IPCC Glossary"
  source_label <- if (identical(source, "IPBES")) "Assessments" else "Reports"
  source_class <- if (identical(source, "IPBES")) "ipbes" else "ipcc"

  empty_ui <- htmltools::div(
    class = paste("glossary-source-section", paste0("glossary-source-", source_class)),
    htmltools::h4(.glossary_section_header_title("Term", source_title)),
    htmltools::div(class = "glossary-empty", "No definitions available.")
  )
  if (!nzchar(concept_id) || is.na(concept_id)) return(list(ui = empty_ui, data = NULL))

  df <- multilingual[
    toupper(trimws(as.character(multilingual$source))) == source &
      trimws(as.character(multilingual$concept_id)) == trimws(as.character(concept_id)),
    ,
    drop = FALSE
  ]
  if (nrow(df) == 0) return(list(ui = empty_ui, data = NULL))

  reports <- trimws(as.character(df$report_or_assessment))
  reports[is.na(reports) | !nzchar(reports)] <- "UNSPECIFIED"
  report_levels <- unique(reports)
  source_data <- list()
  cards <- list()

  main_rows <- df[tolower(trimws(as.character(df$language))) == main_language & as.logical(df$is_available), , drop = FALSE]
  title_term <- if (nrow(main_rows) > 0 && nzchar(trimws(as.character(main_rows$term[[1]])))) {
    trimws(as.character(main_rows$term[[1]]))
  } else {
    en_rows <- df[tolower(trimws(as.character(df$language))) == "en" & as.logical(df$is_available), , drop = FALSE]
    if (nrow(en_rows) > 0) trimws(as.character(en_rows$term[[1]])) else "Term"
  }

  for (r in report_levels) {
    report_df <- df[reports == r, , drop = FALSE]
    report_lang <- tolower(trimws(as.character(report_df$language)))
    report_term <- trimws(as.character(report_df$term))
    report_def <- trimws(as.character(report_df$definition))
    report_avail <- as.logical(report_df$is_available)
    available_langs <- unique(report_lang[
      !is.na(report_lang) &
        !is.na(report_avail) &
        report_avail &
        !is.na(report_term) &
        nzchar(report_term) &
        !is.na(report_def) &
        nzchar(report_def)
    ])
    available_langs <- available_langs[available_langs %in% languages]
    display_langs <- if (length(available_langs) > 0) languages else character(0)
    if (length(display_langs) == 0) {
      cards[[length(cards) + 1L]] <- htmltools::div(
        class = "glossary-def-card glossary-multilingual-card",
        htmltools::div(
          class = "glossary-def-meta",
          htmltools::tags$strong(style = "font-size:inherit; font-weight:700;", paste0(source_label, ": ")),
          .glossary_source_inline(r)
        ),
        htmltools::div(class = "glossary-empty", "No translations available for this report.")
      )
      next
    }

    lang_cards <- lapply(display_langs, function(lang) {
      row <- report_df[tolower(trimws(as.character(report_df$language))) == lang, , drop = FALSE]
      has_text <- FALSE
      term <- ""
      definition <- ""
      if (nrow(row) > 0) {
        available <- as.logical(row$is_available[[1]])
        term <- trimws(as.character(row$term[[1]]))
        definition <- trimws(as.character(row$definition[[1]]))
        if (is.na(term)) term <- ""
        if (is.na(definition)) definition <- ""
        has_text <- isTRUE(available) && nzchar(term) && nzchar(definition)
      }
      if (isTRUE(has_text)) {
        source_data[[length(source_data) + 1L]] <<- data.frame(
          source = source,
          report_or_assessment = r,
          language = lang,
          definition = definition,
          stringsAsFactors = FALSE
        )
      }
      def_html <- if (isTRUE(has_text)) {
        .glossary_highlight_definition_map(definition, dict, hover_lookup)
      } else {
        "Translation not available."
      }
      htmltools::div(
        class = "glossary-lang-card",
        htmltools::div(
          class = "glossary-lang-header",
          htmltools::tags$span(class = "glossary-lang-pill", toupper(lang)),
          htmltools::tags$strong(class = "glossary-lang-term", if (nzchar(term)) term else "Not available")
        ),
        htmltools::div(
          class = "glossary-lang-definition",
          htmltools::HTML(def_html)
        )
      )
    })

    cards[[length(cards) + 1L]] <- htmltools::div(
      class = "glossary-def-card glossary-multilingual-card",
      htmltools::div(
        class = "glossary-def-meta",
        htmltools::tags$strong(style = "font-size:inherit; font-weight:700;", paste0(source_label, ": ")),
        .glossary_source_inline(r)
      ),
      htmltools::div(class = "glossary-lang-grid", lang_cards)
    )
  }

  section_class <- paste("glossary-source-section", paste0("glossary-source-", tolower(source)))
  section_style <- if (identical(source, "IPBES")) {
    "background:#f2fbf3; border-color:#d7efd9;"
  } else {
    "background:#f2f8ff; border-color:#d6e7fb;"
  }

  ui <- htmltools::div(
    class = section_class,
    style = section_style,
    htmltools::h4(.glossary_section_header_title(title_term, source_title)),
    cards
  )

  list(
    ui = ui,
    data = if (length(source_data) == 0) NULL else do.call(rbind, source_data)
  )
}

.glossary_highlight_dictionary_map <- function(multilingual, catalog, mode = "both") {
  if (is.null(catalog) || nrow(catalog) == 0 || is.null(multilingual) || nrow(multilingual) == 0) {
    return(list(terms = character(0), patterns = character(0), targets = character(0)))
  }
  df <- multilingual
  df$source <- toupper(trimws(as.character(df$source)))
  df$language <- tolower(trimws(as.character(df$language)))
  df$term <- trimws(as.character(df$term))
  df$is_available <- as.logical(df$is_available)
  key <- paste(df$source, df$concept_id, sep = "::")
  allowed <- as.character(catalog$value)
  df <- df[key %in% allowed & df$is_available & nzchar(df$term), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(terms = character(0), patterns = character(0), targets = character(0)))
  }

  map <- data.frame(
    term = as.character(df$term),
    target = paste(df$source, df$concept_id, sep = "::"),
    stringsAsFactors = FALSE
  )
  norm <- normalise_term(map$term)
  keep <- !duplicated(norm)
  map <- map[keep, , drop = FALSE]
  ord <- order(-nchar(map$term), tolower(map$term), map$term)
  map <- map[ord, , drop = FALSE]
  escaped <- vapply(map$term, .glossary_escape_regex, character(1), USE.NAMES = FALSE)
  escaped <- gsub("\\s+", "\\\\s+", escaped)
  patterns <- paste0("(?<![[:alnum:]])", escaped, "(?![[:alnum:]])")
  list(terms = map$term, patterns = patterns, targets = map$target)
}

.glossary_hover_lookup_from_catalog <- function(multilingual, catalog, mode = "both") {
  if (is.null(catalog) || nrow(catalog) == 0 || is.null(multilingual) || nrow(multilingual) == 0) return(character(0))
  .glossary_hover_lookup_subset(multilingual, catalog$value)
}

.glossary_hover_lookup_subset <- function(multilingual, keys) {
  if (is.null(multilingual) || nrow(multilingual) == 0 || is.null(keys) || length(keys) == 0) return(character(0))
  keys <- unique(as.character(keys))
  keys <- keys[nzchar(keys)]
  if (length(keys) == 0) return(character(0))

  out <- stats::setNames(rep("No definition available.", length(keys)), keys)
  for (i in seq_along(keys)) {
    key <- keys[[i]]
    parts <- strsplit(key, "::", fixed = TRUE)[[1]]
    if (length(parts) != 2) next
    cid <- parts[[2]]
    src <- toupper(trimws(as.character(parts[[1]])))
    sub <- multilingual[
      toupper(trimws(as.character(multilingual$source))) == src &
        trimws(as.character(multilingual$concept_id)) == cid,
      ,
      drop = FALSE
    ]
    if (nrow(sub) == 0) next
    sub$definition <- trimws(as.character(sub$definition))
    sub <- sub[!is.na(sub$definition) & nzchar(sub$definition), , drop = FALSE]
    if (nrow(sub) == 0) next
    def <- gsub("\\s+", " ", sub$definition[[1]])
    if (!is.na(def) && nchar(def) > 180) def <- paste0(substr(def, 1, 177), "...")
    out[[key]] <- paste0(src, ": ", def)
  }
  out
}

.glossary_highlight_definition_map <- function(text, dict, hover_lookup) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return("\u2014")
  if (is.null(dict) || length(dict$terms) == 0 || length(dict$patterns) == 0 || length(dict$targets) == 0) {
    return(as.character(htmltools::htmlEscape(text)))
  }
  txt <- as.character(text)
  n <- nchar(txt)
  occupied <- rep(FALSE, n)
  found <- list()

  for (i in seq_along(dict$terms)) {
    term <- dict$terms[[i]]
    pattern <- dict$patterns[[i]]
    target <- dict$targets[[i]]
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
        term = term,
        target = target
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
    target <- as.character(m$target)
    if (start > cursor) {
      parts <- c(parts, as.character(htmltools::htmlEscape(substr(txt, cursor, start - 1L))))
    }

    matched <- substr(txt, start, end)
    tooltip <- if (!is.null(names(hover_lookup)) && target %in% names(hover_lookup)) {
      hover_lookup[[target]]
    } else {
      NA_character_
    }
    if (is.null(tooltip) || !nzchar(tooltip)) tooltip <- "No definition available."
    tooltip_attr <- as.character(htmltools::htmlEscape(tooltip))
    tooltip_attr <- gsub("\n", "&#10;", tooltip_attr, fixed = TRUE)

    parts <- c(parts, paste0(
      "<a href=\"#\" class=\"glossary-term-link\" data-term=\"",
      as.character(htmltools::htmlEscape(target)),
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

.glossary_find_keys_in_text <- function(text, dict) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(character(0))
  if (is.null(dict) || length(dict$terms) == 0 || length(dict$patterns) == 0 || length(dict$targets) == 0) return(character(0))
  txt <- as.character(text)
  n <- nchar(txt)
  if (n < 1L) return(character(0))
  occupied <- rep(FALSE, n)
  found <- list()
  for (i in seq_along(dict$terms)) {
    mm <- gregexpr(dict$patterns[[i]], txt, perl = TRUE, ignore.case = TRUE)[[1]]
    if (length(mm) == 1 && mm[[1]] == -1) next
    ll <- attr(mm, "match.length")
    for (j in seq_along(mm)) {
      start <- as.integer(mm[[j]])
      mlen <- as.integer(ll[[j]])
      end <- start + mlen - 1L
      if (start < 1L || end < start || end > n) next
      if (any(occupied[start:end])) next
      occupied[start:end] <- TRUE
      found[[length(found) + 1L]] <- list(start = start, target = dict$targets[[i]])
    }
  }
  if (length(found) == 0) return(character(0))
  found <- found[order(vapply(found, function(x) x$start, integer(1)))]
  vals <- vapply(found, function(x) as.character(x$target), character(1))
  vals[!duplicated(vals)]
}

.glossary_collect_link_keys <- function(source_sections, mode, dict) {
  collect_from_df <- function(detail_df) {
    if (is.null(detail_df) || nrow(detail_df) == 0 || !("definition" %in% names(detail_df))) return(character(0))
    defs <- trimws(as.character(detail_df$definition))
    defs <- defs[!is.na(defs) & nzchar(defs)]
    if (length(defs) == 0) return(character(0))
    vals <- unlist(lapply(defs, .glossary_find_keys_in_text, dict = dict), use.names = FALSE)
    vals[!duplicated(vals)]
  }
  ipbes_keys <- if (!is.null(source_sections$ipbes)) collect_from_df(source_sections$ipbes) else character(0)
  ipcc_keys <- if (!is.null(source_sections$ipcc)) collect_from_df(source_sections$ipcc) else character(0)
  all <- c(ipbes_keys, ipcc_keys)
  if (length(all) > 0) all <- all[!duplicated(all)]
  list(ipbes = ipbes_keys, ipcc = ipcc_keys, all = all)
}

.glossary_label_for_key <- function(key, catalog, multilingual, main_language) {
  row <- .glossary_catalog_row(catalog, key)
  if (is.null(row) || nrow(row) == 0) return(key)
  src <- row$source[[1]]
  cid <- row$concept_id[[1]]
  main <- row$label[[1]]
  sub <- multilingual[
    toupper(trimws(as.character(multilingual$source))) == src &
      trimws(as.character(multilingual$concept_id)) == cid &
      as.logical(multilingual$is_available),
    ,
    drop = FALSE
  ]
  if (nrow(sub) == 0) return(main)
  sub$language <- tolower(trimws(as.character(sub$language)))
  sub$term <- trimws(as.character(sub$term))
  sub <- sub[nzchar(sub$term), , drop = FALSE]
  if (nrow(sub) == 0) return(main)
  others <- unique(sub$term[sub$language != main_language & sub$term != main])
  if (length(others) == 0) return(main)
  paste0(main, " (", paste(others, collapse = ", "), ")")
}

.glossary_see_also_ui_multilingual <- function(links, catalog, multilingual, main_language) {
  if (is.null(links) || length(links$all) == 0) {
    return(htmltools::div(
      class = "glossary-source-section glossary-see-also",
      htmltools::h4("See also"),
      htmltools::div(class = "glossary-empty", "No linked glossary terms in the definitions shown above.")
    ))
  }
  labels <- vapply(links$all, .glossary_label_for_key, character(1), catalog = catalog, multilingual = multilingual, main_language = main_language)
  ord <- order(tolower(labels), labels)
  vals <- links$all[ord]
  labels <- labels[ord]

  all_links <- lapply(seq_along(vals), function(i) {
    htmltools::tagList(
      htmltools::tags$a(
        href = "#",
        class = "glossary-term-link glossary-see-link",
        `data-term` = vals[[i]],
        labels[[i]]
      ),
      if (i < length(vals)) htmltools::HTML(", ") else NULL
    )
  })
  htmltools::div(
    class = "glossary-source-section glossary-see-also",
    htmltools::h4("See also"),
    htmltools::div(class = "glossary-see-links", all_links)
  )
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
