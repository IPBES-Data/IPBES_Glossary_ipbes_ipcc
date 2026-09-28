# glossary.ipbes.ipcc

<!-- badges: start -->
[![R CMD CHECK](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This app was created iteratively with both **Claude Code** and **Codex**
assistance. Contributor details, model/mode metadata, and session history are
documented in `CONTRIBUTORS.md` and `AI_PROMPTS.md`.

An R package containing the **Glossary Explorer**, a Shiny app for exploring the
[IPBES](https://www.ipbes.net/) biodiversity glossary and the
[IPCC](https://www.ipcc.ch/) climate change glossary.

> **Note:** this branch carries the glossary explorer only. The former
> side-by-side comparison app (`run_app()`, similarity scores, word-level diffs
> and the term hierarchy graph) has been removed.

## Features

- source selector (`IPBES`, `IPCC`, `Both`) with autocomplete term lookup
- definitions grouped per assessment (IPBES) or report (IPCC), with identical
  definitions from several sources shown once
- in-definition highlighting of glossary terms; hover to preview a definition
  and click to navigate to it
- a `See also` panel listing every glossary term linked from the definitions
  shown
- case-insensitive term matching and source-specific rendering
- IPCC report sources shown with full report names (from bundled mapping)
- in-app `About` modal and footer `GitHub Issues` link

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("rkrug/glossary_ipbes_ipcc")
```

## Usage

```r
glossary.ipbes.ipcc::run_glossary()
```

The app stores its cache (updated IPCC data, merged glossary cache) in
`tools::R_user_dir("glossary.ipbes.ipcc", "cache")`. No manual setup is
required.

> If that cache directory contains an `ipcc_glossary.csv` from an earlier live
> update, the bundled fast-start cache is bypassed and the app rebuilds
> everything on launch (several minutes). Delete that file to restore instant
> startup.

## Hosted app

- Glossary explorer: https://ipbes-data.shinyapps.io/glossary-ipbes-ipcc-explorer/

## Tests

```r
devtools::test()
```

The suite covers the data pipeline (load, summarise, merge), the term catalog
and lookup, definition highlighting and in-definition linking, hover previews,
see-also derivation, the startup and highlight caches, and the app's server
logic end to end via `shiny::testServer()`.

## Detailed technical background

See [BACKGROUND.md](BACKGROUND.md).

## Deploying to shinyapps.io

```bash
SHINYAPPS_ACCOUNT=... SHINYAPPS_APP_NAME=... \
Rscript scripts/deploy_shinyapps_glossary.R
```

This deploys `app_glossary.R` as a shinyapps.io app.

## Data sources

| Source | URL | Bundled snapshot |
|--------|-----|-----------------|
| IPBES Glossary | https://www.ipbes.net/glossary | `inst/extdata/ipbes_glossary.csv` (2026-02-23, 2,228 terms) |
| IPCC Glossary  | https://apps.ipcc.ch/glossary/ | `inst/extdata/ipcc_glossary.csv` (run `data-raw/prepare_data.R` to regenerate) |

## Developer notes

### Regenerating the bundled data and caches

```r
source("data-raw/prepare_data.R")     # re-scrape IPCC, rebuild everything
```

```bash
Rscript inst/scripts/update_bundled_caches.R --force   # caches only, no scrape
Rscript inst/scripts/scrape_ipcc_and_update_caches.R   # scrape + caches
```

Commit the regenerated `inst/extdata/` artifacts to git.

Bump `.HIGHLIGHT_CACHE_VERSION` in `R/app_glossary.R` whenever the rendered
definition HTML or the see-also derivation changes, otherwise a stale cache is
treated as current and the change never reaches the app.

### AI development log

See `AI_PROMPTS.md` for the full prompt history and design decisions.

## License

- Source code: MIT © 2026 Rainer M Krug (Rainer@krugs.de)
- Documentation and background content: CC BY 4.0
  (see [LICENSE-CC-BY-4.0.md](LICENSE-CC-BY-4.0.md))
