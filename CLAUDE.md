# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`glossary.ipbes.ipcc` is an R package providing two Shiny web applications:
- **Comparison app** (`run_app()`): side-by-side glossary comparison with similarity scores, word-level diffs, and a directed hierarchy graph
- **Glossary explorer** (`run_glossary()`): term browser with source filtering, autocomplete, and in-definition term navigation

## Common Commands

```r
# Run apps locally
glossary.ipbes.ipcc::run_app()
glossary.ipbes.ipcc::run_glossary()

# Build and check
devtools::build()
devtools::check()

# Regenerate bundled IPCC data (takes several minutes, scrapes apps.ipcc.ch)
source("data-raw/prepare_data.R")

# Run tests
devtools::test()
testthat::test_file("tests/testthat/test-<name>.R")
```

```bash
# Deploy to shinyapps.io
Rscript scripts/deploy_shinyapps_compare.R   # comparison app
Rscript scripts/deploy_shinyapps_glossary.R  # glossary explorer
```

## Architecture

### Data Flow

```
inst/extdata/ipbes_glossary.csv  ──┐
inst/extdata/ipcc_glossary.csv   ──┤
  (or scraped via apps.ipcc.ch)    │
                                   ↓
                         load_ipbes() / load_ipcc()   [R/data_ipbes.R, R/data_ipcc.R]
                                   ↓
                         merge_glossaries()            [R/data_merge.R]
                         (2-pass: exact match + qualifier-stripped)
                                   ↓
                         compute_similarity_triplet()  [R/similarity_text.R]
                         compute_term_hierarchy()      [R/hierarchy_terms.R]
                                   ↓
                         Triple-tier cache:
                           1. packaged (inst/extdata/*.rds)
                           2. user startup (~/.Rdata/glossary.../*.rds)
                           3. full rebuild
                                   ↓
                         Shiny apps (UI + server modules)
```

### Key Modules

| File | Role |
|------|------|
| `R/app.R` | Entry points, caching orchestration, runtime detection |
| `R/app_glossary.R` | Glossary explorer Shiny app (UI + server) |
| `R/data_merge.R` | Full outer join with two-pass term matching |
| `R/similarity_text.R` | TF-based cosine similarity (base R, no API needed) |
| `R/hierarchy_terms.R` | Directed subsumption scores for hierarchy graph |
| `R/diff_text.R` | LCS word-level diff for definition comparisons |
| `R/mod_table.R` | Comparison table with expandable rows (reactable) |
| `R/mod_graph.R` | Interactive hierarchy graph |
| `R/utils.R` | HTML cleaning, term normalization helpers |

### Caching

User cache lives in `tools::R_user_dir("glossary.ipbes.ipcc", "cache")`. Load order: packaged `.rds` → user startup cache (if source signatures match) → full rebuild. The "Update IPCC" live-update button is enabled locally but disabled on shinyapps.io by default; override with env var `GLOSSARY_ENABLE_LIVE_UPDATE=0|1`.

### Data Sources

- **IPBES**: Bundled CSV snapshot (`inst/extdata/ipbes_glossary.csv`, ~2,228 terms from 2026-02-23)
- **IPCC**: Scraped from `apps.ipcc.ch` AJAX endpoints via `data-raw/prepare_data.R`; result committed to `inst/extdata/ipcc_glossary.csv`

## Deployment

Both apps are deployed to shinyapps.io. The deploy scripts read credentials from env vars (`SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`, `SHINYAPPS_APP_NAME`). See `scripts/deploy_shinyapps_*.R` and `manifest.json`.

## Development Notes

- Roxygen2 (`RoxygenNote: 7.3.3`) for docs; run `devtools::document()` after changing `@` tags
- AI development history is in `AI_PROMPTS.md` (3 sessions with Claude Code / Codex)
- `BACKGROUND.md` has a technical deep-dive into similarity algorithms and caching design
