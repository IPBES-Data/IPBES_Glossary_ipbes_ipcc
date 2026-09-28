# Detailed Background: IPBES <-> IPCC Glossary Explorer

## 1. Purpose

This package provides one Shiny app, the glossary explorer (`run_glossary()`),
which offers source-filtered lookup of IPBES and IPCC glossary terms with
linked in-definition term navigation.

> This branch carries the explorer only. The former comparison app (`run_app()`)
> and everything that existed solely to support it -- TF-cosine similarity
> scoring, LCS word-level diffs, the reactable comparison table, and the
> directed term hierarchy graph -- have been removed. Sections 6, 7, 8, 10 and
> 11 of the previous revision of this document covered those and are gone.

## 2. Data Sources and Artifacts

### 2.1 Bundled package snapshots

- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/ipcc_report_names.csv`
- `inst/extdata/merged_glossary_cache.rds`

These are shipped with the package and available on first startup.

`ipcc_report_names.csv` maps IPCC report abbreviations (for example `AR6`,
`SR15`) to their long report names for UI display.

### 2.2 Runtime cache (user cache dir)

The app also uses:

- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/ipcc_glossary.csv`
- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/startup_merged_cache.rds`

An `ipcc_glossary.csv` in the user cache takes precedence over the bundled
snapshot. Note the consequence: while such a file is present the bundled
`merged_glossary_cache.rds` is bypassed entirely and the app rebuilds
everything on launch, which takes several minutes. Delete it to restore
instant startup.

## 3. Scraping (IPCC)

The IPCC glossary is a per-report structure: a term can appear in many reports,
each report may word the definition differently, and some report entries carry
no definition at all. The scraper therefore works at (term, report)
granularity.

`scrape_ipcc()` in `R/data_ipcc.R` is the single implementation;
`data-raw/prepare_data.R` calls it rather than keeping its own copy.

1. Collect term IDs:
   - `ajax.searchbyindex.php?q=<PREFIX>`, parsing `span.alllink[data-phraseid]`
2. Fetch all occurrences of each term:
   - `ajax.searchalloccurance.php?q=<PHRASE_ID>&r=`
   - `.ipcc_parse_occurrences()` reads **every** `<dd>` block: the
     `data-report` attribute gives the report, the first `<p>` gives that
     report's own definition
3. Fill in cross-references for report entries with no definition:
   - `ajax.searchbyphraseandreport.php?q=<PHRASE_ID>&r=<REPORT>`
   - `.ipcc_parse_cross_reference()` returns the kind (`see` / `see_also`),
     the target term and its phrase id
4. Write one row per (term, report):
   `id`, `prefix`, `term`, `report`, `definition`, `xref_kind`,
   `xref_target`, `xref_target_id`, `downloaded_at`

Politeness: `Sys.sleep(0.3)` between requests, 15 s timeout.

Term labels have working-group suffixes (`<< WGI >>` and guillemet forms)
stripped.

### 3.1 Why per-report matters

An earlier version read only `def_nodes[[1]]` -- the first `<dd>` -- and stored
one definition per term, which `summarise_ipcc()` then repeated across every
report in the list. Measured on the 2026-05-06 snapshot: 1,532 terms, 3,380
(term, report) pairs, 804 terms (52%) appearing in more than one report and
covering 2,652 pairs. In a sample of 8 multi-report terms, all 8 had genuinely
different wording per report -- `Likelihood` has 9 reports and 8 distinct
definitions. The app was attributing one report's text to all of them.

The per-report definitions were always present in the bulk response, so
capturing them costs no extra requests.

### 3.2 Related-term pointers

Each report entry can carry pointers to other terms, served only by
`ajax.searchbyphraseandreport.php` and absent from the bulk response. The
`ul.items` list is used for three different relationships, told apart by the
`<h6>` heading above it and the prefix on each `<li>`:

| Kind | Heading / prefix | Meaning |
|------|------------------|---------|
| `see` | `See...` | a redirect to another term |
| `see_also` | `See Also...` | a supplementary pointer |
| `sub_terms` | `Sub-terms` heading, no prefix | the narrower terms beneath this one |

The kind must be read from the markup, never inferred from whether the
definition is blank. A report entry may have a definition, pointers, both, or
neither -- `Extreme climate event` in AR6 has a 730-character definition *and* a
`See` redirect, and `Ice sheet` in SRCCL has a definition plus
`See Also... Glacier`. Measured on a 60-pair sample, **35% of defined
(term, report) pairs carry pointers.**

Pointers vary per report: `Atmosphere-ocean general circulation model` is
`See Climate model` in SRCCL but `See Climate model (spectrum or hierarchy)` in
AR5-WG2.

Storage is `xref_kind`, `xref_target` and `xref_target_id`, with multiple
targets joined by `" | "`. **That pipe is a storage delimiter and must never
reach the UI** -- `.ipcc_xref_display()` renders the display string, labelling
by kind and joining with commas. A redirect wins when a row carries both kinds.

Because a defined entry can also carry pointers, filling them requires the
per-report endpoint for every pair, not just the definition-less ones:
`.ipcc_fill_cross_references(scope = "all")`. `scope = "undefined"` is the
cheaper pass that captures redirects only.

## 4. Loading and Cleaning

### 4.1 IPBES loading

- Reads CSV from bundled snapshot.
- Lowercases concept names so case variants group together.
- Normalizes key columns (concept, definition, deliverables, etc.).
- Splits multi-assessment entries into one row per assessment.
- Cleans HTML/entities with `clean_html()`.

### 4.2 IPCC loading

- Uses cache file first, then bundled snapshot.
- Ensures required columns exist.
- Cleans definitions with `clean_html()`.
- Reads the per-(term, report) format directly. Legacy snapshots in the older
  one-row-per-term shape (with a semicolon `reports` column) are still accepted:
  `.ipcc_expand_legacy_reports()` expands them, repeating the single stored
  definition across reports, so a stale user cache keeps working.
- `summarise_ipcc()` keeps definitions exactly as the report words them and
  carries the pointer display string in a separate `xref` column. Pointers are
  IPCC's own editorial data and are shown apart from the definition, never
  spliced into the quoted text.

## 5. Merging Logic

Merge is a full outer merge over normalized term keys:

1. Exact key match: `normalise_term(ipbes_concept)` vs `normalise_term(ipcc_term)`
2. Fallback for unmatched IPBES term:
   - removes parenthetical qualifier, e.g. `abundance (ecological)` -> `abundance`
3. Remaining unmatched rows are kept as IPBES-only or IPCC-only entries.

Output includes:

- term labels and counts (`ipbes_n_assessments`, `ipcc_n_reports`)
- detailed list-columns (`ipbes_data`, `ipcc_data`)

## 6. Rendering a Term

For the selected term and source mode the app renders one section per source.
Within a section, definitions that are textually identical across several
assessments or reports are collapsed into a single card listing all of them.

### 6.1 In-definition term linking

Every glossary term occurring inside a definition is turned into a link that
navigates to that term. Matching is:

- case-insensitive, but the original casing in the text is preserved
- constrained to whole words (`(?<![[:alnum:]]) ... (?![[:alnum:]])`)
- longest-match-first, so `ecosystem services` wins over `services`
- non-overlapping: once a span is claimed no shorter term can re-match it

Definition text is HTML-escaped; only the generated anchors are markup.

A candidate prefilter narrows the ~3,000 patterns to those whose first token
occurs in the text. That prefilter must tokenize terms with the same
`[[:alnum:]]+` rule used on the text. Splitting terms on whitespace instead
produces first words such as `agro-ecological` or `(model)`, which can never
appear in the text's alphanumeric token set -- previously silencing roughly 300
terms. See `.glossary_highlight_dictionary()`.

### 6.2 Related-term pointers vs. the See also panel

These are two different things and are deliberately kept apart:

- **Per-card pointers** (`.glossary-def-xref`) are IPCC's own report-specific
  cross-references, rendered inside each definition card below the definition
  and visually separated from it. Their targets are linked like any glossary
  term.
- **The `See also` panel** at the foot of the page is derived, not authored: it
  lists every glossary term that happens to occur in the definitions currently
  shown, across both sources, via `.glossary_collect_link_terms()`. It is not
  fed by IPCC's pointers, precisely because it is cross-source while they are
  IPCC-specific.

Cards are grouped on definition **and** pointers, so two reports sharing a
definition but pointing elsewhere stay separate entries. A card with pointers
but no definition shows "Listed in:" rather than "As defined in:".

## 7. Startup and Caching Behavior

Startup data load order:

1. packaged merged cache (`inst/extdata/merged_glossary_cache.rds`) if valid
2. user startup cache (`startup_merged_cache.rds`) if source signatures match
3. full rebuild (load IPBES, load IPCC, merge)

The packaged cache is validated by the md5 sums of the two source CSVs; the
user startup cache by their path, size and mtime.

### 7.1 Highlight cache

The rendered `definition_html` for every definition and the `see_also_list` for
every term are pre-computed at build time and stored in the merged cache,
stamped with `.HIGHLIGHT_CACHE_VERSION`. This is what makes term selection
instant instead of compiling ~3,000 regexes on first use.

**Bump `.HIGHLIGHT_CACHE_VERSION` whenever the rendered HTML or the see-also
derivation changes.** `.has_current_highlight_cache()` only checks that
constant, so without a bump a stale cache is treated as current and the change
never reaches the app -- a correct source tree and an unchanged UI.

## 8. Running Locally

```r
glossary.ipbes.ipcc::run_glossary()
```

### 8.1 Deploy entrypoint for shinyapps.io

- `app_glossary.R` (explorer)
- `app.R` (Posit Connect / Posit Cloud)

### 8.2 Refresh packaged snapshots for release

```bash
Rscript inst/scripts/update_bundled_caches.R --force   # caches only
Rscript inst/scripts/scrape_ipcc_and_update_caches.R   # scrape + caches
```

or, for a full rebuild including the IPBES copy:

```r
source("data-raw/prepare_data.R")
```

## 9. Tests

`devtools::test()` runs the suite in `tests/testthat/`:

| File | Covers |
|------|--------|
| `test-utils.R` | HTML cleaning, term normalisation, qualifier stripping |
| `test-data-loading.R` | IPBES/IPCC load and summarise, source path resolution |
| `test-merge.R` | outer join, qualifier-stripped pass, unmatched rows |
| `test-term-catalog.R` | catalog construction, row lookup, choice resolution |
| `test-highlighting.R` | dictionary, matching rules, escaping, term finding |
| `test-grouping-and-sources.R` | definition grouping, report-name expansion |
| `test-hover-and-see-also.R` | hover previews, see-also derivation |
| `test-highlight-cache.R` | highlight cache stamping and invalidation |
| `test-startup-cache.R` | signatures, round-trip, rejection, packaged cache |
| `test-server.R` | server logic end to end via `shiny::testServer()` |
| `test-terms-without-definitions.R` | regression cover for the cross-reference gap |

Fixtures in `helper-fixtures.R` push small synthetic glossaries through the
real load -> summarise -> merge pipeline rather than hand-building merged
tables, so they exercise the same code path the app uses at startup.
