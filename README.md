# glossary.ipbes.ipcc

<!-- badges: start -->
[![R CMD CHECK](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rkrug/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This app was created iteratively with both **Claude Code** and **Codex**
assistance. Contributor details, model/mode metadata, and session history are
documented in `CONTRIBUTORS.md` and `AI_PROMPTS.md`.

An R package containing a Shiny app for comparing the
[IPBES](https://www.ipbes.net/) biodiversity glossary and the
[IPCC](https://www.ipcc.ch/) climate change glossary side-by-side.

## Features

- **Side-by-side comparison** table with one row per glossary term
- **Expandable rows**: click any term to see definitions broken down by IPBES
  assessment and IPCC report
- **Word-level text diff**: colour-coded differences between IPBES and IPCC
  definitions
- **Similarity scoring**: within-source and between-source text similarity
- **Sort alphabetically** (default) and sortable similarity columns
- **Update IPCC glossary** button that re-scrapes the live
  [IPCC Glossary website](https://apps.ipcc.ch/glossary/) with real-time
  progress feedback

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("rkrug/glossary_ipbes_ipcc")
```

## Usage

```r
glossary.ipbes.ipcc::run_app()
```

The app stores its cache (updated IPCC data, merged table cache) in
`tools::R_user_dir("glossary.ipbes.ipcc", "cache")`.  No manual setup is
required.

## Detailed technical background

For a full implementation walkthrough (scraping, merge logic, similarity
methods, word-level diff interpretation, caching, and local vs hosted behavior),
see [BACKGROUND.md](BACKGROUND.md).

## Deploying to shinyapps.io

Use the deploy helper script:

```bash
SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
Rscript scripts/deploy_shinyapps.R
```

This deploys `app.R` and sets hosted-safe mode by default
through runtime detection on shinyapps.io, which disables live IPCC scraping
for hosted instances. Local runs keep live update enabled by default.

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
