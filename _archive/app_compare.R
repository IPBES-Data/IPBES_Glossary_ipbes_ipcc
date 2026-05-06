# shinyapps.io entrypoint
#
# This file is used by rsconnect/shinyapps.io deployments.
# It sources package R files directly from the repository checkout and returns
# a shinyApp object.

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

cache_dir <- tools::R_user_dir("glossary.ipbes.ipcc", which = "cache")

app <- .create_shiny_app(
  cache_dir = cache_dir,
  enable_live_update = .default_live_update_enabled()
)

app
