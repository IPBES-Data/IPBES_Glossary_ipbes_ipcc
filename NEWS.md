# glossary.ipbes.ipcc 0.7.0

## Improvements

* Graph tab redesign and navigation:
  - graph is now the primary view with tree-focused controls
  - added deterministic tree navigation (`Focus Previous Tree` / `Focus Next Tree`)
  - keeps top-level trees ordered left-to-right by descending tree size (ties alphabetically)
* Graph readability and interaction updates:
  - improved root label visibility and placement above root nodes
  - preserved node label styling during selection/proxy updates
  - added source-based node shapes (`IPBES`, `IPCC`, `IPBES + IPCC`)
  - switched to built-in `visNetwork` legend for node-shape mapping
* Table behavior updates for node-driven workflows:
  - graph selection now highlights only the selected term in the Glossary table
  - highlighted rows are sorted to the top in both:
    - Glossary Table
    - Top Directed Edges
* Export and reporting improvements:
  - added export support (HTML/PDF) for graph settings and selected-tree tables
  - added bundled export template under `inst/rmarkdown/`
* Bundled cache and data-refresh tooling:
  - added bundled hierarchy cache snapshot (`inst/extdata/hierarchy_edges_cache.rds`)
  - added `inst/scripts/update_bundled_caches.R`
  - added `inst/scripts/scrape_ipcc_and_update_caches.R`
  - deployment script validates bundled cache presence before deployment

# glossary.ipbes.ipcc 0.6.0

## Improvements

* Added directed term hierarchy scoring (`compute_term_hierarchy()`) that builds
  parent -> child edges using lexical subsumption, definition containment, and
  cosine definition similarity.
* Added a new `Graph` tab with interactive hierarchy visualization:
  - node labels and hover tooltips with merged definitions
  - click-to-select tree highlighting
  - non-linked node/edge grey-out
  - "Focus Selected Tree" control to fit the selected connected subtree
* Added cross-tab highlighting so graph selections are reflected in the main
  glossary table.
* Added hierarchy edge caching with source fingerprint invalidation to avoid
  recomputing hierarchy scores on each refresh.
* Improved graph readability and behavior:
  - increased horizontal spacing (`nodeSpacing`/`treeSpacing`)
  - preserved pan/zoom during selection style updates via proxy updates
  - fixed module-namespaced proxy targeting for robust highlighting/fit actions
* Added explicit documentation that tokenization uses no stemming/lemmatization
  (for example, `impact` and `impacts` are treated as different tokens) in:
  - `R/similarity_text.R` docs
  - `BACKGROUND.md`
  - `vignettes/background.Rmd` and rendered `inst/www/background.html`

# glossary.ipbes.ipcc 0.5.0

## Improvements

* Added detailed technical background documentation as a package vignette:
  - `vignettes/background.Rmd`
  - rendered app-accessible HTML at `inst/www/background.html`
* Added in-app `Info` button opening the technical background in a new tab.
* Added app branding and attribution:
  - SIB logo
  - copyright notice for Rainer M Krug
* Added repository and issue links:
  - updated all links to `https://github.com/rkrug/glossary_ipbes_ipcc`
  - added top-of-app GitHub repo/issues links
* Added dynamic app version display under the main title:
  - shown as `Version <DESCRIPTION version>`
* Removed the `Sort Alphabetically` button and its server wiring to avoid
  crash-on-click behavior.
* Addressed package check/documentation quality items:
  - fixed roxygen argument documentation mismatches
  - regenerated affected `man/*.Rd` files
  - normalized R source to ASCII where required
  - cleaned `.Rbuildignore` entries for local hidden/development directories
  - aligned DESCRIPTION metadata (`Author`/`Maintainer`, URL/BugReports)

# glossary.ipbes.ipcc 0.2.1

## Improvements

* Added hosted-safe runtime behavior for shinyapps.io deployments:
  - automatic hosted runtime detection
  - live IPCC refresh can be disabled in hosted mode
  - explicit override via `GLOSSARY_ENABLE_LIVE_UPDATE`
* Added a top-level `app.R` deployment entrypoint for shinyapps.io/rsconnect.
* Added `scripts/deploy_shinyapps.R` to support reproducible CLI deployments.
* Added `.rsconnect/` to `.gitignore`.
* Updated deployment documentation in `README.md`.

# glossary.ipbes.ipcc 0.2.0

## Improvements

* Major startup performance improvements:
  - Bundled merged cache snapshot in `inst/extdata/merged_glossary_cache.rds`
  - Startup cache loading and invalidation metadata
  - Source-package path fallback to use local `inst/` data when running from checkout
* IPCC scraper switched to the full `search.php` endpoint family (all reports).
* Overview table and detail rendering updated:
  - Grouped identical definitions in overview
  - Pairwise similarity matrix in expanded rows
  - Lazy detail rendering to avoid large initial payloads
* Similarity display and layout refined:
  - Within-similarity indicators inside definition columns
  - Between-similarity shown in dedicated Similarity column
  - Similarity column moved to second position
* Removed `Sort by Similarity` control from the toolbar (table remains sortable by column headers).
* Removed unused Wikipedia similarity wiring and legacy cache helpers.
* Updated footer IPCC link to `https://apps.ipcc.ch/glossary/search.php`.
* Updated styling and alignment for definition/assessment-report text.

# glossary.ipbes.ipcc 0.1.0

## New features

* Initial release of the IPBES/IPCC glossary comparison Shiny app.
* Loads IPBES glossary from bundled CSV (2,228 terms across 13+ assessments).
* Bundles pre-scraped IPCC glossary snapshot (run `data-raw/prepare_data.R`
  to regenerate).
* Side-by-side comparison table with expandable rows (click any row to expand).
* Per-assessment IPBES definitions and per-report IPCC definitions in detail
  view.
* Word-level text diff (LCS algorithm) with colour-coded `<del>`/`<ins>`
  highlighting.
* "Update IPCC Glossary" button with live `withProgress()` feedback that
  re-scrapes the IPCC website.
* "Sort Alphabetically" (default) button.
* `run_app()` entry point accepting a `cache_dir` argument (defaults to
  `tools::R_user_dir("glossary.ipbes.ipcc", "cache")`).
