# AI Development Prompts

This file records all prompts and design decisions made with AI assistance,
so that development can be resumed at any time with any AI agent.

---

## Session 1 — 2026-02-27

**Tool**: Claude Code
**Model**: claude-sonnet-4-6
**Mode**: Plan mode (plan written and approved before any code was generated)

### Initial request (verbatim)

> I would like to have a shiny app which compares the IPCC and IPBES
> glossaries. The IPBES Glossary is available in an exported csv file in the
> folder IPBES, while the IPCC glossary needs to be scraped from the website.
> Claude gave me the code in IPCC/download_ipcc_glossary.R to do that. The
> Shiny App should do the following:
>
> 1. Give a table where one row is one definition with the following columns:
>    1. IPBES definition
>       1. one sub column for each definition of an assessment if multiple ones are there
>    2. IPCC definition
>       1. one sub column for each definition of different reports
>    3. Summarising the differences between the definitions
> 2. Sort the table alphabetically
> 3. Add a button to sort the table by "similarity" of the term. This should
>    not use the definitions provided, but definitions provided in general
>    (e.g. wikipedia or other resources)
> 4. Add a button to update the IPCC glossary from the one packaged
>
> Package the shiny app in a package form and prepare it for publishing it.

### Clarifying questions and answers

**Q: Table layout?**
A: Collapsed with expandable rows
- Main row: Term | IPBES summary | IPCC summary | Similarity %
- Expanded: per-assessment IPBES defs, per-report IPCC defs, word-level diff

**Q: Summarising differences column approach?**
A: Text diff + similarity score (algorithmic, no API key needed)

**Q: Similarity computation approach?**
A: On-demand via Wikipedia API (fetch summary per term, compute TF-IDF cosine
similarity, cache results to disk across sessions)

**Q: Publishing target?**
A: GitHub only (install via `remotes::install_github()`)

**Q: Package name?** (requested after initial plan)
A: `glossary.ipbes.ipcc`

### Additional features requested (after initial plan)

- R-conform `NEWS.md` (updated each session)
- R-conform `CONTRIBUTORS.md` mentioning Claude Code with model version and mode
- Claude Code listed in `Authors@R` in DESCRIPTION with model/mode in comment
- This `AI_PROMPTS.md` file documenting all prompts for AI continuity
- Both `NEWS.md` and `AI_PROMPTS.md` kept updated each session

### Key design decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Table component | `reactable` | Native expandable row support via `details` |
| Text similarity | TF-IDF cosine (base R) | No API key, works offline |
| Wikipedia role | Neutral reference for similarity | Avoids circular domain-specific comparison |
| IPCC caching | `tools::R_user_dir()` cache | Never mutates `inst/extdata/` at runtime |
| `diffobj` dep | `Suggests` (optional) | Falls back to built-in LCS diff |
| `shinyjs` dep | `Suggests` (optional) | Graceful degradation without it |
| Publishing | GitHub only | No CRAN compliance overhead |

### Architecture summary

```
run_app()
  ├── load_ipbes()      # reads inst/extdata/ipbes_glossary.csv
  ├── load_ipcc()       # reads cache or inst/extdata/ipcc_glossary.csv
  ├── merge_glossaries() # outer join on normalised term names
  └── shinyApp(
        ui     = build_ui(),
        server = build_server(cache_dir, initial_data)
      )
          ├── mod_table_server()       # reactable with detail rows
          └── mod_update_ipcc_server() # scrape button with progress
```

### Critical files

- `data-raw/IPBES/glossary_2026-02-23.csv` — IPBES source (moved from `IPBES/`)
- `data-raw/IPCC/download_ipcc_glossary.R` — IPCC scraper (moved from `IPCC/`)
- `data-raw/prepare_data.R` — **must be run once** before `devtools::build()`
  to populate `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/ipcc_glossary.csv` — bundled IPCC snapshot (generated, then committed)
- `R/data_merge.R` — core join logic
- `R/mod_table.R` — main UI component

---

## Session 2 — 2026-02-27

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Fix HTML/tag rendering issues in overview and expanded diff blocks
- Improve typography/readability and table layout
- Switch IPCC scraper/update flow to `search.php` endpoint family (all reports)
- Clarify and refine similarity metrics:
  - within IPBES
  - within IPCC
  - between all definitions
- Rework expanded detail view into a pairwise matrix and reduce duplication
- Improve startup and refresh performance with packaged and startup caches
- Keep runtime update behavior while preserving package snapshots
- Remove unstable/unused similarity UI controls and Wikipedia-specific wiring
- Final layout refinements (column ordering, alignment, compact indicators)

### Key implementation decisions

- Startup cache precedence:
  1. packaged merged cache (`inst/extdata/merged_glossary_cache.rds`)
  2. user startup cache (`startup_merged_cache.rds`)
  3. rebuild from source CSVs
- Source-run compatibility:
  - added fallback path resolution to local `inst/` when `system.file()` is empty
- Table performance:
  - precompute table HTML fragments with cache versioning
  - lazy-load expanded detail content
- Similarity model simplification:
  - use in-package text similarity metrics from glossary definitions
  - remove unused Wikipedia similarity runtime path

### Files substantially updated in this session

- `R/app.R`
- `R/data_ipcc.R`
- `R/mod_table.R`
- `R/mod_update_ipcc.R`
- `R/server.R`
- `R/ui.R`
- `R/data_merge.R`
- `inst/www/custom.css`
- `data-raw/prepare_data.R`
- `inst/extdata/merged_glossary_cache.rds`

### Release target

- Package release prepared as `0.2.0`

---

## Session 3 — 2026-02-27

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Make hosted deployments safe for shinyapps.io while keeping local update
  functionality.
- Add a deployment script for shinyapps.io.
- Prepare release metadata for version `0.2.1`.

### Key implementation decisions

- Added runtime environment detection and default live-update toggle:
  - local default: live update enabled
  - hosted shinyapps default: live update disabled
  - explicit env override through `GLOSSARY_ENABLE_LIVE_UPDATE`
- Added top-level `app.R` as `appPrimaryDoc` for rsconnect deployments.
- Added `scripts/deploy_shinyapps.R`:
  - reads `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`
  - deploys with `envVars = "GLOSSARY_ENABLE_LIVE_UPDATE"`
  - defaults hosted deployments to `GLOSSARY_ENABLE_LIVE_UPDATE=0`

### Files substantially updated in this session

- `R/app.R`
- `R/server.R`
- `R/ui.R`
- `app.R`
- `scripts/deploy_shinyapps.R`
- `README.md`
- `inst/www/custom.css`
- `.gitignore`
- `DESCRIPTION`
- `NEWS.md`
- `CONTRIBUTORS.md`

### Release target

- Package release prepared as `0.2.1`

---

## Session 4 — 2026-02-27

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Add a detailed background technical document and convert it into a vignette.
- Expose the vignette in the app via an `Info` button.
- Add copyright attribution and SIB branding (including logo) in the app.
- Improve app metadata and links:
  - add repository/issues links in app UI
  - switch links to `rkrug/glossary_ipbes_ipcc`
  - show package version dynamically in app header (`Version <x>`).
- Remove unstable `Sort Alphabetically` button and server wiring due to crash.
- Resolve package check/documentation issues and re-run checks.

### Key implementation decisions

- Vignette implementation:
  - source at `vignettes/background.Rmd`
  - rendered HTML copy at `inst/www/background.html` for direct app linking
  - `Info` button opens the HTML in a new browser tab.
- Version display:
  - prefer `utils::packageVersion("glossary.ipbes.ipcc")`
  - fallback to local `DESCRIPTION` when running from source checkout.
- Link strategy:
  - top-of-app repo/issues links for quick access
  - footer links retained and aligned to the same repo/issues target.
- Check cleanup:
  - fixed roxygen argument doc mismatches
  - normalized non-ASCII characters in R source
  - cleaned build-ignore entries for local hidden directories.

### Files substantially updated in this session

- `R/ui.R`
- `R/server.R`
- `R/app.R`
- `R/data_ipbes.R`
- `R/data_ipcc.R`
- `R/data_merge.R`
- `R/diff_text.R`
- `inst/www/custom.css`
- `inst/www/background.html`
- `vignettes/background.Rmd`
- `BACKGROUND.md`
- `README.md`
- `DESCRIPTION`
- `NEWS.md`
- `CONTRIBUTORS.md`
- `.Rbuildignore`
- `man/*.Rd` (regenerated by roxygen)

### Release target

- Package release prepared as `0.5.0`

---

## Session 5 — 2026-03-04

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Add a directed term hierarchy model for matched terms.
- Add an interactive `Graph` tab to visualize hierarchy edges.
- Improve graph startup/runtime performance with hierarchy edge caching.
- Add node-selection behavior:
  - highlight selected connected tree
  - grey out non-linked nodes/edges
  - highlight associated rows in the main table
- Add explicit tree focus control (`Focus Selected Tree`).
- Fix graph regressions:
  - robust click event handling
  - correct namespaced `visNetworkProxy()` updates inside module server
  - preserve pan/zoom while updating selection styles
- Update background technical docs to clarify tokenization behavior and graph
  hierarchy method.

### Key implementation decisions

- Directed hierarchy score:
  - `score = 0.55 * lex_sub + 0.25 * def_contain + 0.20 * def_sim`
- Candidate gating:
  - parent must be shorter (`parent_token_n < child_token_n`)
  - lexical/definition containment gate before full score
- Cache-first graph approach:
  - compute full hierarchy once (`min_score = 0`, no parent pruning)
  - cache edges in user cache dir
  - filter thresholds interactively in-memory
- Interaction architecture:
  - shared `highlight_terms_rv` between graph and table modules
  - graph styling updates via `visNetworkProxy()` (`visUpdateNodes/Edges`)
  - table row highlighting by selected connected terms

### Files substantially updated in this session

- `R/hierarchy_terms.R` (new)
- `R/mod_graph.R` (new)
- `R/server.R`
- `R/mod_table.R`
- `R/ui.R`
- `R/similarity_text.R`
- `inst/www/custom.css`
- `BACKGROUND.md`
- `vignettes/background.Rmd`
- `inst/www/background.html`
- `DESCRIPTION`
- `NEWS.md`
- `AI_PROMPTS.md`

### Release target

- Package release prepared as `0.6.0`

---

## Session 6 — 2026-03-04

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Refine Graph tab tree navigation and readability without changing package version.
- Remove edge-count truncation control and show all filtered hierarchy edges.
- Add tree-to-tree navigation controls:
  - `Focus Previous Tree`
  - `Focus Next Tree`
- Keep top-level tree ordering deterministic:
  - larger trees on the left
  - ties resolved alphabetically by root term
- Improve root label readability and placement (above root nodes).
- Preserve node font styling during selection/highlight proxy updates.

### Key implementation decisions

- Keep changes narrow to avoid regressions in existing graph interactions:
  - no structural changes to scoring logic
  - no changes to package version metadata
- Apply visual updates through existing `visNetwork` node fields and proxy style
  payloads, while retaining pan/zoom behavior.
- Document graph UX and ordering behavior in `NEWS`, `BACKGROUND`, and vignette
  docs for reproducibility.

### Files substantially updated in this session

- `R/mod_graph.R`
- `inst/www/custom.css`
- `NEWS.md`
- `AI_PROMPTS.md`
- `BACKGROUND.md`
- `vignettes/background.Rmd`

### Release target

- Follow-up development commit on top of `0.6.0` (no version bump)

---

## Session 7 — 2026-03-04

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Finalize graph-first UX and release readiness for `0.7.0`.
- Improve graph semantics and readability:
  - add source-based node shapes (`IPBES`, `IPCC`, `IPBES + IPCC`)
  - use `visNetwork` built-in legend in the graph
- Align node selection behavior across tables:
  - only selected node highlighted in Glossary table
  - highlighted rows sorted to top in Glossary and Top Directed Edges tables
- Add bundled cache maintenance tooling:
  - script to rebuild bundled caches from packaged CSV sources
  - script to scrape IPCC glossary and immediately refresh bundled caches
- Keep startup and deployment cache-safe by bundling hierarchy cache and
  validating required cache artifacts during deployment.

### Files substantially updated in this session

- `R/mod_graph.R`
- `R/mod_table.R`
- `R/ui.R`
- `R/server.R`
- `R/app.R`
- `inst/www/custom.css`
- `inst/rmarkdown/graph_export_report.Rmd`
- `inst/scripts/update_bundled_caches.R`
- `inst/scripts/scrape_ipcc_and_update_caches.R`
- `scripts/deploy_shinyapps.R`
- `data-raw/prepare_data.R`
- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/merged_glossary_cache.rds`
- `inst/extdata/hierarchy_edges_cache.rds`
- `BACKGROUND.md`
- `vignettes/background.Rmd`
- `inst/www/background.html`
- `DESCRIPTION`
- `NEWS.md`
- `CONTRIBUTORS.md`
- `AI_PROMPTS.md`

### Release target

- Package release prepared as `0.7.0`

---

## Session 8 — 2026-03-04

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Added and refined a second Shiny app for focused glossary exploration:
  - `run_glossary()` API entry point
  - source selector (`IPBES`, `IPCC`, `Both`)
  - source-scoped term selector behavior
  - case-insensitive term resolution and de-duplication
  - click-through navigation via highlighted in-definition terms
- Updated glossary explorer app metadata and branding:
  - title: `IPBES and IPCC Glossary explorer`
  - dynamic package version display from `DESCRIPTION`
  - glossary/repo/issues links
  - copyright + SIB logo attribution
- Split shinyapps deployment entrypoints into explicit compare/explorer files:
  - renamed top-level compare entrypoint `app.R` -> `app_compare.R`
  - added `app_glossary.R`
  - renamed compare deploy helper to `scripts/deploy_shinyapps_compare.R`
  - added glossary deploy helper `scripts/deploy_shinyapps_glossary.R`
- Prepared release metadata and docs for version `0.8.0`.

### Files substantially updated in this session

- `R/app_glossary.R`
- `R/app.R`
- `NAMESPACE`
- `man/run_glossary.Rd`
- `app_compare.R`
- `app_glossary.R`
- `scripts/deploy_shinyapps_compare.R`
- `scripts/deploy_shinyapps_glossary.R`
- `inst/www/custom.css`
- `DESCRIPTION`
- `README.md`
- `NEWS.md`
- `CONTRIBUTORS.md`
- `BACKGROUND.md`
- `vignettes/background.Rmd`
- `inst/www/background.html`
- `AI_PROMPTS.md`

### Release target

- Package release prepared as `0.8.0` (no commit yet)

---

## Session 9 — 2026-03-19

**Tool**: Codex  
**Model**: GPT-5  
**Mode**: Default mode

### Main requests handled

- Prepared `0.9.8` release of glossary explorer updates.
- Included finalized glossary explorer UI/content adjustments already present in
  working tree:
  - About modal text refresh
  - footer Issues-link placement
  - dynamic glossary section titles with selected-term emphasis
  - merged + alphabetically sorted `See also` links
  - institutional naming update to `Senckenberg Biodiversity and Climate`
- Updated release/docs metadata for this release:
  - `DESCRIPTION` version -> `0.9.8`
  - top `NEWS.md` entry for `0.9.8`
  - `README.md` glossary explorer feature notes
  - `BACKGROUND.md` glossary explorer UX release notes

### Files substantially updated in this session

- `R/app_glossary.R`
- `inst/www/about_glossary.html`
- `inst/www/custom.css`
- `DESCRIPTION`
- `NEWS.md`
- `README.md`
- `BACKGROUND.md`
- `AI_PROMPTS.md`

### Release target

- Package release prepared as `0.9.8` and deployed glossary explorer app to
  `ipbes-data/glossary-ipbes-ipcc-explorer`

---

## Session 10 — 2026-03-23

**Tool**: Codex  
**Model**: GPT-5  
**Mode**: Default mode

### Main requests handled

- Added bundled IPCC report abbreviation mapping as package data:
  - `inst/extdata/ipcc_report_names.csv`
- Added report-name expansion helpers and wired display formatting to use long
  IPCC report names in:
  - glossary explorer definition metadata
  - comparison/table source labels
- Fixed lookup fallback to avoid `subscript out of bounds` when an unknown
  report abbreviation is encountered.
- Prepared release metadata/docs for `1.0.0`.

### Files substantially updated in this session

- `R/ipcc_report_names.R`
- `R/app_glossary.R`
- `R/mod_table.R`
- `inst/extdata/ipcc_report_names.csv`
- `DESCRIPTION`
- `NEWS.md`
- `README.md`
- `BACKGROUND.md`
- `AI_PROMPTS.md`

### Release target

- Package release prepared as `1.0.0` and deployed to shinyapps.io

---

## How to continue development with another AI agent

Provide this file and the approved plan at
`.claude/plans/async-roaming-hinton.md` to the AI, along with the current
repo state. The AI should:

1. Read `AI_PROMPTS.md` (this file) for full context and decisions
2. Read `.claude/plans/async-roaming-hinton.md` for the implementation plan
3. Check `NEWS.md` to understand what has been implemented so far
4. Run `data-raw/prepare_data.R` if `inst/extdata/ipcc_glossary.csv` is a stub
5. Continue implementing from the next incomplete step in the plan
6. Update `NEWS.md` and `AI_PROMPTS.md` at the end of the session

---

*This file is part of the package and ships to end users as documentation.*
