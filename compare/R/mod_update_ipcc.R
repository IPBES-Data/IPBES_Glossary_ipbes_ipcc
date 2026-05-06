# Shiny module: IPCC update button
# =============================================================================

#' UI for the IPCC update module
#'
#' @param id Shiny module namespace id.
#' @return A `tagList` with the update button, progress placeholder, and status
#'   text.
#' @keywords internal
mod_update_ipcc_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::actionButton(
      ns("update_btn"),
      label = "Update IPCC Glossary",
      icon  = shiny::icon("download"),
      class = "btn btn-primary"
    ),
    shiny::uiOutput(ns("progress_ui")),
    shiny::textOutput(ns("update_status"))
  )
}

# =============================================================================

#' Server for the IPCC update module
#'
#' @param id        Shiny module namespace id.
#' @param cache_dir Path to the user cache directory.
#' @param merged_rv A [shiny::reactiveVal()] to update after a successful scrape.
#' @keywords internal
mod_update_ipcc_server <- function(id, cache_dir, merged_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Show last-updated timestamp on startup
    output$update_status <- shiny::renderText({
      cache_file <- file.path(cache_dir, "ipcc_glossary.csv")
      if (file.exists(cache_file)) {
        mtime <- format(file.info(cache_file)$mtime, "%Y-%m-%d %H:%M")
        paste0("Cache last updated: ", mtime)
      } else {
        "Using bundled IPCC snapshot."
      }
    })

    shiny::observeEvent(input$update_btn, {
      # Disable button during update
      shiny::updateActionButton(session, "update_btn",
                                label = "Updating\u2026",
                                icon  = shiny::icon("spinner"))

      shiny::withProgress(
        message = "Updating IPCC glossary\u2026",
        value   = 0,
        {
          result <- tryCatch({
            scrape_ipcc(
              cache_dir         = cache_dir,
              progress_callback = function(current, total, term) {
                shiny::incProgress(
                  amount  = 1 / max(total, 1),
                  message = paste0("Fetching: ", term,
                                   " (", current, "/", total, ")")
                )
              }
            )
            "success"
          }, error = function(e) {
            paste0("Error: ", conditionMessage(e))
          })
        }
      )

      if (identical(result, "success")) {
        # Reload and re-merge
        bundled_ipbes <- .pkg_file("extdata", "ipbes_glossary.csv")
        bundled_ipcc  <- .pkg_file("extdata", "ipcc_glossary.csv")
        new_ipcc   <- load_ipcc(cache_dir, bundled_path = bundled_ipcc)
        ipbes_long <- load_ipbes(path = bundled_ipbes)
        new_merged <- merge_glossaries(
          summarise_ipbes(ipbes_long),
          summarise_ipcc(new_ipcc)
        )
        new_merged <- .prepare_table_data(new_merged)
        .save_startup_merged_cache(
          cache_dir = cache_dir,
          meta      = .build_startup_cache_meta(cache_dir),
          merged    = new_merged
        )
        merged_rv(new_merged)

        output$update_status <- shiny::renderText({
          paste0("Updated successfully: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
        })
      } else {
        output$update_status <- shiny::renderText({ result })
        shiny::showNotification(
          result, type = "error", duration = 10
        )
      }

      # Re-enable button
      shiny::updateActionButton(session, "update_btn",
                                label = "Update IPCC Glossary",
                                icon  = shiny::icon("download"))
    })
  })
}
