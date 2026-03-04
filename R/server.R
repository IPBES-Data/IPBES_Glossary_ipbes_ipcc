# Shiny server definition
# =============================================================================

#' Build the Shiny application server function
#'
#' @param cache_dir   Path to the cache directory.
#' @param initial_data A pre-loaded merged glossary [tibble::tibble()] from
#'   [merge_glossaries()].
#' @param enable_live_update Logical; if `FALSE`, disables the live IPCC update
#'   module for hosted-safe deployments.
#' @return A Shiny `server` function.
#' @keywords internal
build_server <- function(cache_dir, initial_data, enable_live_update = TRUE) {
  function(input, output, session) {

    # -- Shared reactive state -----------------------------------------------
    merged_rv  <- shiny::reactiveVal(initial_data)
    highlight_terms_rv <- shiny::reactiveVal(character(0))

    output$similarity_status <- shiny::renderText({
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) return("")
      n_available <- sum(!is.na(data$sim_between_all))
      paste0("Between similarity available for ", n_available, " terms")
    })

    # -- IPCC update module --------------------------------------------------
    if (isTRUE(enable_live_update)) {
      mod_update_ipcc_server("update_ipcc", cache_dir, merged_rv)
    }

    # -- Hierarchy graph module ----------------------------------------------
    mod_graph_server(
      id = "main_graph",
      merged_rv = merged_rv,
      cache_dir = cache_dir,
      highlight_terms_rv = highlight_terms_rv
    )
  }
}
