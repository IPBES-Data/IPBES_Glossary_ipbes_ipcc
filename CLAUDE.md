# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`glossary.ipbes.ipcc` provides one Shiny web application:

- **Glossary explorer** (`run_glossary()`): term browser with source filtering
  (IPBES / IPCC / Both), autocomplete, definitions grouped per assessment or
  report, and in-definition term navigation.

> This branch carries the explorer only. The former comparison app (`run_app()`)
> and its supporting code -- similarity scoring, word-level diffs, the reactable
> comparison table, and the directed term hierarchy graph -- have been removed.

## Common Commands

```r
# Run the app locally
glossary.ipbes.ipcc::run_glossary()

# Tests
devtools::test()
testthat::test_file("tests/testthat/test-<name>.R")

# Build and check
devtools::document()
devtools::check()

# Regenerate bundled IPCC data (several minutes, scrapes apps.ipcc.ch)
source("data-raw/prepare_data.R")
```

```bash
# Rebuild bundled caches only (no scrape, ~2.5 min)
Rscript inst/scripts/update_bundled_caches.R --force

# Scrape IPCC then rebuild caches
Rscript inst/scripts/scrape_ipcc_and_update_caches.R

# Deploy to shinyapps.io
Rscript scripts/deploy_shinyapps_glossary.R
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
                         .prepare_glossary_highlight_data()
                         (pre-renders definition_html + see_also_list)
                                   ↓
                         Triple-tier cache:
                           1. packaged (inst/extdata/merged_glossary_cache.rds)
                           2. user startup (R_user_dir cache)
                           3. full rebuild
                                   ↓
                         Glossary explorer Shiny app
```

### Key Modules

| File | Role |
|------|------|
| `R/app.R` | Shared helpers: caching orchestration, package paths, version |
| `R/app_glossary.R` | The app: UI, server, term lookup, highlighting, see-also |
| `R/data_ipbes.R` | IPBES load + summarise |
| `R/data_ipcc.R` | IPCC load + summarise + `scrape_ipcc()` and its HTML parsers |
| `R/data_merge.R` | Full outer join with two-pass term matching |
| `R/ipcc_report_names.R` | Report abbreviation to long-name expansion |
| `R/utils.R` | HTML cleaning, term normalisation helpers |

### Caching

User cache lives in `tools::R_user_dir("glossary.ipbes.ipcc", "cache")`. Load
order: packaged `.rds` → user startup cache (if source signatures match) → full
rebuild.

Two traps worth remembering:

- A leftover `ipcc_glossary.csv` in the user cache dir makes the app prefer it
  over the bundled snapshot, which **bypasses the packaged cache entirely** and
  forces a multi-minute rebuild on every launch.
- `.HIGHLIGHT_CACHE_VERSION` in `R/app_glossary.R` must be bumped whenever the
  rendered definition HTML or the see-also derivation changes.
  `.has_current_highlight_cache()` checks only that constant, so without a bump
  a stale cache is served and the change never appears in the app.

### IPCC data is per-report

The IPCC glossary is a per-(term, report) structure, and
`inst/extdata/ipcc_glossary.csv` holds one row per pair. A term can be defined
differently in each report -- 570 of 758 multi-report terms (75%) are -- so
never collapse the reports to a single definition.

Each report entry can also carry related-term pointers in `xref_kind` /
`xref_target` / `xref_target_id`, of three kinds: `see` (redirect), `see_also`
(supplementary) and `sub_terms` (narrower terms). The kind comes from the
markup, never from whether the definition is blank: an entry may have a
definition, pointers, both, or neither, and 35% of defined pairs carry pointers.

Two rules that are easy to get wrong:

- `xref_target` joins multiple targets with `" | "`. That is a **storage
  delimiter only**; `.ipcc_xref_display()` produces the user-facing string.
  Never render `xref_target` raw.
- Pointers are displayed in their own block inside each definition card
  (`.glossary-def-xref`), separated from the definition. They must **not** be
  spliced into the definition text, and they must **not** feed the `See also`
  panel -- that panel is derived across both sources from terms occurring in
  definition text, while pointers are IPCC-specific.

### Encoding: always build in a UTF-8 locale

A bare `Rscript` usually runs in the `C` locale, where R cannot represent
non-ASCII text natively and `write.csv()` silently replaces each character with
a literal `<U+XXXX>` escape -- which `clean_html()` then strips as if it were an
HTML tag. This corrupted degree signs, en-dashes, curly quotes and
transliterated Sanskrit in both snapshots for a long time before it was caught.

`.ensure_utf8_locale()` now guards `scrape_ipcc()` and both cache-building
scripts and errors if no UTF-8 locale can be set. If you invoke anything that
writes `inst/extdata/` by another route, prefix it:

```bash
LC_ALL=en_US.UTF-8 Rscript <script>
```

`tests/testthat/test-encoding.R` asserts the bundled snapshots stay clean.

## Tests

`tests/testthat/` covers the data pipeline, term catalog and lookup,
highlighting, hover previews, see-also, both caches, and the server logic via
`shiny::testServer()`. Fixtures in `helper-fixtures.R` build small synthetic
glossaries and push them through the real load → summarise → merge pipeline.

## Development Notes

- Roxygen2 (`RoxygenNote: 7.3.3`) for docs; run `devtools::document()` after changing `@` tags
- AI development history is in `AI_PROMPTS.md`
- `BACKGROUND.md` has a technical deep-dive into the merge, rendering and caching design
