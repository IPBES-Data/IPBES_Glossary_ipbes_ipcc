# shinyapps.io entrypoint for glossary explorer
#
# This file is used by rsconnect/shinyapps.io deployments.
# It sources package R files directly from the repository checkout and returns
# a shinyApp object for the glossary explorer.

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

cache_dir <- tools::R_user_dir("glossary.ipbes.ipcc", which = "cache")

app <- .create_glossary_app(cache_dir = cache_dir)

app
