# Shiny UI definition
# =============================================================================

#' Build the Shiny application UI
#'
#' @param enable_live_update Logical; if `FALSE`, the IPCC live update control
#'   is replaced with a hosted-safe informational label.
#' @return A [shiny::fluidPage()] UI object.
#' @keywords internal
build_ui <- function(enable_live_update = TRUE) {
  sib_logo_url <- "https://www.sib.swiss/sites/default/files/2023-08/sib_logo2023.png"
  repo_url <- "https://github.com/rkrug/glossary_ipbes_ipcc"
  issues_url <- "https://github.com/rkrug/glossary_ipbes_ipcc/issues"
  app_version <- .app_version_string()

  update_ui <- if (isTRUE(enable_live_update)) {
    mod_update_ipcc_ui("update_ipcc")
  } else {
    htmltools::span(
      class = "update-disabled",
      "Hosted mode: IPCC live update disabled"
    )
  }

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(
        rel  = "stylesheet",
        href = "custom/custom.css"
      )
    ),

    shiny::titlePanel(
      title = NULL,
      windowTitle = "IPBES \u2194 IPCC Glossary Comparison"
    ),

    htmltools::div(
      htmltools::h2(
        "IPBES \u2194 IPCC Glossary Comparison",
        style = "margin-bottom:0;"
      ),
      htmltools::p(
        style = "color:#555; font-size:1.2rem; font-weight:600; margin:2px 0 0 0;",
        paste0("Version ", app_version)
      ),
      htmltools::p(
        style = "color:#666; font-size:1rem; margin-top:4px;",
        "Side-by-side comparison of IPBES and IPCC glossary definitions."
      ),
      htmltools::p(
        style = "color:#666; font-size:0.95rem; margin-top:2px;",
        "GitHub Repo: ",
        htmltools::a("rkrug/glossary_ipbes_ipcc",
                     href = repo_url,
                     target = "_blank"),
        " | Issues: ",
        htmltools::a("GitHub Issues",
                     href = issues_url,
                     target = "_blank")
      )
    ),

    # ---- Toolbar: links + update button -----------------------------------
    shiny::fluidRow(
      shiny::column(
        width = 12,
        htmltools::div(
          class = "glossary-toolbar",

          htmltools::tags$a(
            class = "btn btn-default",
            href = "custom/background.html",
            target = "_blank",
            shiny::icon("info-circle"),
            "Info"
          ),

          # IPCC update module UI
          update_ui,

          # Right-aligned status message
          htmltools::div(
            class = "update-status",
            shiny::textOutput("similarity_status", inline = TRUE)
          )
        )
      )
    ),

    shiny::hr(style = "margin:0.5rem 0 1rem 0;"),

    # ---- Main content: graph + selected-tree tables -----------------------
    shiny::fluidRow(
      shiny::column(
        width = 12,
        mod_graph_ui("main_graph")
      )
    ),

    # ---- Footer ------------------------------------------------------------
    shiny::hr(),
    htmltools::div(
      style = "color:#888; font-size:0.9rem; padding-bottom:1rem;",
      "Data: ",
      htmltools::a("IPBES Glossary", href = "https://www.ipbes.net/glossary",
                   target = "_blank"),
      " | ",
      htmltools::a("IPCC Glossary", href = "https://apps.ipcc.ch/glossary/search.php",
                   target = "_blank"),
      " | GitHub Repo: ",
      htmltools::a("glossary.ipbes.ipcc",
                   href = repo_url,
                   target = "_blank"),
      " | Issues: ",
      htmltools::a("GitHub Issues",
                   href = issues_url,
                   target = "_blank")
    ),
    htmltools::div(
      class = "app-attribution",
      htmltools::img(
        src = sib_logo_url,
        alt = "SIB Swiss Institute of Bioinformatics",
        class = "sib-logo"
      ),
      htmltools::div(
        class = "copyright-note",
        htmltools::HTML("&copy; 2026 Rainer M Krug (Rainer.Krug@sib.swiss) &middot; "),
        htmltools::a(
          "SIB Swiss Institute of Bioinformatics",
          href = "https://www.sib.swiss/",
          target = "_blank"
        )
      )
    )
  )
}

.app_version_string <- function() {
  .package_version_safe()
}
