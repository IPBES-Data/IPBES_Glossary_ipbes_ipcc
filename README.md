# glossary.ipbes.ipcc

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19181890.svg)](https://doi.org/10.5281/zenodo.19181890)
[![Open Shiny App](https://img.shields.io/badge/Connect%20Cloud-Open%20App-blue)](https://019d19d3-2a50-2200-2ed0-301b5602a6da.share.connect.posit.cloud)
[![R CMD CHECK](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This app was created iteratively with both **Claude Code** and **Codex**
assistance. Contributor details, model/mode metadata, and session history are
documented in `CONTRIBUTORS.md` and `AI_PROMPTS.md`.

An R package containing two Shiny apps for comparing and exploring the
[IPBES](https://www.ipbes.net/) biodiversity glossary and the
[IPCC](https://www.ipcc.ch/) climate change glossary.

## Features

- **Comparison app** (`run_app()`):
  - side-by-side glossary comparison with grouped definitions
  - within-source and between-source similarity metrics
  - word-level differences and directed term hierarchy graph
  - optional live IPCC refresh (hosted-safe behavior on shinyapps.io)
- **Glossary explorer app** (`run_glossary()`):
  - source selector (`IPBES`, `IPCC`, `Both`) with autocomplete term lookup
  - in-definition highlighting of glossary terms
  - hover to preview definitions and click highlighted terms to navigate
  - case-insensitive term matching and source-specific rendering
  - in-app `About` modal and footer `GitHub Issues` link for user guidance
  - IPCC report sources shown with full report names (from bundled mapping)

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("rkrug/glossary_ipbes_ipcc")
```

## Usage

```r
# Comparison app
glossary.ipbes.ipcc::run_app()

# Glossary explorer app
glossary.ipbes.ipcc::run_glossary()
```

The app stores its cache (updated IPCC data, merged table cache) in
`tools::R_user_dir("glossary.ipbes.ipcc", "cache")`.  No manual setup is
required.

## Hosted apps

- Comparison app: https://rmkrug.shinyapps.io/glossary-ipbes-ipcc/
- Glossary explorer app: https://ipbes-data.shinyapps.io/glossary-ipbes-ipcc-explorer/

## Detailed technical background

For a full implementation walkthrough (scraping, merge logic, similarity
methods, word-level diff interpretation, caching, and local vs hosted behavior),
see [BACKGROUND.md](BACKGROUND.md).

## Deploying to shinyapps.io

All deploy helpers are branch-safe by default:

- `main`, `master`, and `release*` deploy to the base production app name
- non-production branches (for example `dev`) deploy to a suffixed name
  (for example `glossary-ipbes-ipcc-dev`)
- `SHINYAPPS_APP_NAME` is an explicit override
- `DEPLOY_BRANCH` overrides detected git branch
- `DEPLOY_DRY_RUN=1` prints target resolution without deploying

Use the compare-app deploy helper:

```bash
SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
Rscript scripts/deploy_shinyapps_compare.R
```

This deploys `app_compare.R` and sets hosted-safe mode by default
through runtime detection on shinyapps.io, which disables live IPCC scraping
for hosted instances. Local runs keep live update enabled by default.

Use the glossary-explorer deploy helper:

```bash
SHINYAPPS_ACCOUNT=... SHINYAPPS_APP_NAME=... \
Rscript scripts/deploy_shinyapps_glossary.R
```

This deploys `app_glossary.R` as a separate shinyapps.io app.

Example for `dev` branch target resolution without deploying:

```bash
DEPLOY_BRANCH=dev DEPLOY_DRY_RUN=1 \
Rscript scripts/deploy_shinyapps_compare.R
```

## Deploying to Posit Connect / Connect Cloud

New branch-safe deploy helpers:

- `scripts/deploy_connect_compare.R` (deploys `app_compare.R`)
- `scripts/deploy_connect_glossary.R` (deploys `app_glossary.R`)

Required environment variables:

- `CONNECT_SERVER`
- `CONNECT_API_KEY`

Optional environment variables:

- `CONNECT_SERVER_NAME` (default: `posit-connect`)
- `CONNECT_ACCOUNT` (auto-detected when omitted)
- `CONNECT_CONTENT_NAME` (explicit target-name override)
- `CONNECT_GUID` (existing content id/guid to update)
- `CONNECT_PROD_GUID` / `CONNECT_PROD_GUIDS` (known production guid(s) for guard)
- `DEPLOY_BRANCH` and `DEPLOY_DRY_RUN`
- `ALLOW_PROD_OVERWRITE` (default `0`)

Example for `dev` branch target resolution without deploying:

```bash
CONNECT_SERVER=https://<tenant>.share.connect.posit.cloud \
CONNECT_API_KEY=... \
DEPLOY_BRANCH=dev DEPLOY_DRY_RUN=1 \
Rscript scripts/deploy_connect_glossary.R
```

### Production overwrite prevention

If `CONNECT_GUID` is provided on a non-production branch and matches a known
production GUID (`CONNECT_PROD_GUID` or `CONNECT_PROD_GUIDS`), deployment is
blocked by default. Override intentionally with:

```bash
ALLOW_PROD_OVERWRITE=1
```

## Data sources

| Source | URL | Bundled snapshot |
|--------|-----|-----------------|
| IPBES Glossary | https://www.ipbes.net/glossary | `inst/extdata/ipbes_glossary.csv` (2026-02-23, 2,228 terms) |
| IPCC Glossary  | https://apps.ipcc.ch/glossary/ | `inst/extdata/ipcc_glossary.csv` (run `data-raw/prepare_data.R` to regenerate) |

## Developer notes

### Regenerating the bundled IPCC data

The bundled IPCC CSV must be generated before building the package:

```r
source("data-raw/prepare_data.R")
```

This runs the IPCC scraper (takes several minutes) and writes
`inst/extdata/ipcc_glossary.csv`.  Commit the result to git.

### AI development log

This package was developed with AI assistance. See `AI_PROMPTS.md` for the
full prompt history and design decisions, enabling any AI agent to continue
development.

## License

- Source code: MIT © 2026 Rainer M Krug (Rainer@krugs.de)
- Documentation and background content: CC BY 4.0
  (see [LICENSE-CC-BY-4.0.md](LICENSE-CC-BY-4.0.md))
