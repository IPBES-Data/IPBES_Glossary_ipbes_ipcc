# Posit Connect / Posit Cloud Shiny entrypoint
#
# Platforms that require a top-level app.R use this file.
# Set "Primary file" to app.R in the Posit Connect source settings.

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

cache_dir <- tools::R_user_dir("glossary.ipbes.ipcc", which = "cache")

app <- .create_glossary_app(cache_dir = cache_dir)

app
