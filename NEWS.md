# glossary.ipbes.ipcc 2.0.0

## Breaking changes

* This branch now carries the **glossary explorer only**. The comparison app
  and everything that existed solely to support it have been removed:
  `run_app()`, `render_text_diff()`, `compute_text_similarity()`,
  `compute_term_hierarchy()` and the `tokenise_text()` / `STOPWORDS` /
  `truncate_text()` / `similarity_bar_html()` helpers are gone, along with
  `R/ui.R`, `R/server.R`, `R/mod_graph.R`, `R/mod_table.R`,
  `R/mod_update_ipcc.R`, `R/diff_text.R`, `R/hierarchy_terms.R` and
  `R/similarity_text.R`.
* `merge_glossaries()` no longer computes the `sim_within_ipbes`,
  `sim_within_ipcc` and `sim_between_all` columns; they were only ever read by
  the comparison app, and dropping them removes the O(n^2) pairwise cosine pass
  from a cold merge.
* `inst/extdata/hierarchy_edges_cache.rds` and the graph export template are no
  longer shipped. `reactable` and `visNetwork` are no longer dependencies.

## Bug fixes

* **Non-ASCII characters were silently corrupted in both bundled snapshots.**
  A bare `Rscript` typically runs in the `C` locale, where R cannot represent
  non-ASCII text in the native encoding and `write.csv()` replaces each
  character with a literal `<U+XXXX>` escape. `clean_html()` then stripped that
  escape as if it were an HTML tag, so `1.5°C pathway` became
  `1.5C pathway` and `range of observed values` reached the app as
  `rangeof observed values`. This affected 239 rows of the previous IPCC
  snapshot and 237 rows of the IPBES snapshot (degree signs, en-dashes, curly
  quotes, and transliterated Sanskrit such as `ahimsā`); the original
  IPBES export was clean, so the corruption was introduced by the build.
  - Both snapshots are regenerated in a UTF-8 locale and are now clean.
  - `.ensure_utf8_locale()` is called by `scrape_ipcc()` and by the
    cache-building scripts, and **errors** rather than letting a build write
    mangled data.
  - `clean_html()` normalises `U+00A0` to an ordinary space before stripping
    tags. PCRE's `\s` does not match `U+00A0`, so a non-breaking space inside a
    term also broke whole-word matching.
  - A new test asserts the bundled snapshots contain no `<U+XXXX>` escapes.

* **IPCC definitions were misattributed across reports.** The IPCC glossary is a
  per-(term, report) structure and a term is frequently worded differently in
  each report, but the scraper kept only the first report's definition and
  `summarise_ipcc()` repeated it across every report in the list. 804 of 1,532
  terms (52%) appear in more than one report, covering 2,652 (term, report)
  pairs; in a sample of 8 multi-report terms all 8 differed, with `Likelihood`
  carrying 8 distinct definitions across its 9 reports. The bundled snapshot is
  now one row per (term, report), and each report shows its own wording. The
  per-report text was always present in the bulk response, so this costs no
  extra requests.
* **Related-term pointers are captured and displayed.** IPCC entries carry
  `See` (redirect), `See Also` (supplementary) and `Sub-terms` (narrower terms)
  pointers, all previously discarded. 129 terms rendered as dead ends purely
  because they are redirects. Pointers are now stored in `xref_kind`,
  `xref_target` and `xref_target_id`, and shown in their own block inside each
  definition card, visually separated from the definition and with their targets
  linked. They do **not** feed the `See also` panel, which remains a derived,
  cross-source list of terms occurring in definition text.
  - The kind is read from the markup, not inferred from a blank definition: an
    entry may have a definition, pointers, both, or neither. 35% of defined
    (term, report) pairs carry pointers -- `Extreme climate event` in AR6 has a
    730-character definition *and* a `See` redirect.
  - Pointers vary per report, so definition cards are grouped on definition
    **and** pointers; two reports sharing a definition but pointing elsewhere
    stay separate entries.
  - A card with pointers but no definition is labelled "Listed in:" rather than
    "As defined in:".

* In-definition term linking: terms whose first word contains a non-alphanumeric
  character (`agro-ecological zone`, `(model) ensemble`, `asia-pacific region`,
  ...) were never highlighted. The candidate prefilter compared a
  whitespace-split first word against the definition's alphanumeric tokens, so
  those patterns were filtered out before matching. Roughly 300 of the ~3,000
  catalog terms were affected; 337 IPCC definitions gain links as a result.
  `.HIGHLIGHT_CACHE_VERSION` is bumped to `2L` so existing caches rebuild.
* `.prepare_glossary_highlight_data()` no longer warns once per row about an
  uninitialised `see_also_list` column.

## Data

* The bundled IPCC snapshot is re-scraped as of 2026-09-15: 3,377 (term, report)
  rows covering 1,530 terms, of which 3,142 carry a definition and 210 a
  cross-reference. 570 of 758 multi-report terms (75%) turned out to word the
  definition differently per report -- text that the previous snapshot collapsed
  to a single wording.
* `Indigenous peoples` (previously attributed to AR5-WG2, AR5-WG3 and AR6) has
  been withdrawn from the IPCC glossary upstream and is therefore no longer in
  the app. The website and the IPBES CSV are the authoritative sources, so
  upstream removals propagate.
* The cache rebuild scripts validate a new snapshot before adopting it and fail
  if more than 1% of previously present terms disappear, so a parsing regression
  is caught rather than silently committed.

## Other changes

* `.HIGHLIGHT_CACHE_VERSION` is bumped to `3L`: the pre-rendered definition HTML
  no longer contains pointer text, and a new `xref_html` field is pre-rendered
  alongside it.
* `load_ipcc()` gains `report`, `xref_kind`, `xref_target` and `xref_target_id`
  columns and drops the semicolon-separated `reports` column. Snapshots in the
  old format are still read: their report list is expanded, repeating the single
  stored definition, so an existing user cache keeps working.
* The IPCC scraper existed in two copies that had to be kept in step;
  `data-raw/prepare_data.R` now calls `scrape_ipcc()` instead of duplicating it.
  HTML parsing is factored into `.ipcc_parse_occurrences()` and
  `.ipcc_parse_cross_reference()`.

## Testing

* Added a `testthat` suite (`tests/testthat/`) covering the data pipeline, term
  catalog and lookup, definition highlighting and in-definition linking, hover
  previews, see-also derivation, the startup and highlight caches, and the app's
  server logic end to end via `shiny::testServer()`, and the two IPCC HTML
  parsers against fixture markup.

## Documentation

* `README.md`, `CLAUDE.md`, `BACKGROUND.md` and the background vignette rewritten
  for the explorer-only package, and now document the two caching traps (a stale
  `ipcc_glossary.csv` in the user cache dir bypassing the packaged cache, and
  `.HIGHLIGHT_CACHE_VERSION`) and the uncaptured IPCC cross-reference entries.

# glossary.ipbes.ipcc 1.1.0

## New data

* Added BBA (Business and biodiversity assessment) glossary entries to the
  bundled IPBES data (`inst/extdata/ipbes_glossary.csv`), bringing the total
  to ~2,301 rows.

## Bug fixes

* `load_ipbes()`: concept names are now lowercased on load, so BBA entries
  (e.g. `"Bioeconomy"`) are correctly merged with matching entries from other
  assessments rather than appearing as separate terms.
* Glossary explorer term selector: all terms are now displayed in lowercase,
  eliminating mixed-case duplicates (e.g. `"Biodiversity hotspots"` /
  `"biodiversity hotspots"`).

# glossary.ipbes.ipcc 1.0.0

## Improvements

* Added bundled IPCC report-name mapping data:
  - new file `inst/extdata/ipcc_report_names.csv`
  - abbreviation-to-long-name expansion is now used in displayed report
    source lines.
* Updated glossary explorer and comparison views to display long IPCC report
  names in definition metadata (`As defined in:`).
* Fixed report-name lookup fallback to prevent `subscript out of bounds`
  errors for report codes not present in the mapping file.

# glossary.ipbes.ipcc 0.9.8

## Improvements

* Glossary explorer About modal content refreshed with finalized project
  context and attribution text.
* Moved `GitHub Issues` access from the header to the footer data block.
* Updated glossary section headers to include the selected term and improved
  term emphasis styling.
* Simplified `See also` output into one combined list and sorted linked terms
  alphabetically.
* Updated institution wording in app attribution to
  `Senckenberg Biodiversity and Climate`.

# glossary.ipbes.ipcc 0.9.0

## Improvements

* Updated glossary explorer layout and metadata presentation:
  - moved `Term` and `Source` controls into a single left column
  - set title to `IPBES and IPCC Glossary Explorer` (Title Case)
  - app header now shows `Version <DESCRIPTION version> (D Month YYYY)`
* Refined glossary explorer header/footer links and attribution:
  - removed top GitHub repository link while keeping issue tracker link
  - replaced footer block with:
    `Developed by Rainer M Krug - SIB Swiss Institute of Bioinformatics and Senckenberg Nature Research`
  - made `Rainer M Krug` a single-click mail link addressed to both institutional emails
* Adjusted glossary definition card typography for readability:
  - kept definition body text larger
  - increased `As defined in:` text size while preserving visual hierarchy
  - retained extra vertical spacing between definition body and source line

# glossary.ipbes.ipcc 0.8.0

## Improvements

* Added a second Shiny app entry point: `run_glossary()`.
  - focused glossary explorer workflow with source selector (`IPBES`, `IPCC`, `Both`)
  - term autocomplete with stable selection behavior
  - case-insensitive term resolution for typed and clicked terms
  - in-definition term highlighting with hover tooltips and click-to-navigate
  - source-specific rendering so single-source mode does not show cross-source labels
* Added dedicated shinyapps.io deployment entrypoints and scripts:
  - `app_compare.R` (renamed from top-level `app.R`)
  - `app_glossary.R` (new)
  - `scripts/deploy_shinyapps_compare.R`
  - `scripts/deploy_shinyapps_glossary.R`
* Updated glossary explorer UI metadata and branding:
  - title updated to `IPBES and IPCC Glossary explorer`
  - dynamic package version display (`Version <DESCRIPTION version>`)
  - links to IPBES/IPCC glossary pages, GitHub repo, and issue tracker
  - SIB logo and copyright attribution block aligned with the comparison app
* Updated package documentation and release metadata for the two-app setup.
* Declared minimum R version requirement (`R >= 4.1.0`) in `DESCRIPTION`
  (package code uses base pipe syntax).

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
