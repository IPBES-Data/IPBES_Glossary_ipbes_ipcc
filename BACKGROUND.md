# Detailed Background: IPBES <-> IPCC Glossary Compare and Explore Apps

## 1. Purpose

This package provides two Shiny apps that use shared glossary data:

- comparison app (`run_app()`)
- glossary explorer app (`run_glossary()`)

Both apps work with glossary terms and definitions from:

- IPBES glossary
- IPCC glossary

The comparison app aligns terms, computes similarity metrics, shows word-level
differences, and visualizes directed term hierarchy.
The glossary explorer app provides source-filtered lookup with linked
in-definition term navigation.

## 2. Data Sources and Artifacts

### 2.1 Bundled package snapshots

- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/ipcc_report_names.csv`
- `inst/extdata/merged_glossary_cache.rds`
- `inst/extdata/hierarchy_edges_cache.rds`

These are shipped with the package and available on first startup.

`ipcc_report_names.csv` maps IPCC report abbreviations (for example `AR6`,
`SR15`) to their long report names for UI display.

### 2.2 Runtime cache (user cache dir)

The app also uses:

- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/ipcc_glossary.csv`
- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/startup_merged_cache.rds`

`ipcc_glossary.csv` in the user cache is created/updated by the "Update IPCC
Glossary" button (when enabled). It does not modify `inst/extdata`.

## 3. Scraping (IPCC)

The app and `data-raw/prepare_data.R` scrape the IPCC glossary via the
`search.php` endpoint family (all reports):

1. Collect term IDs:
   - `https://apps.ipcc.ch/glossary/ajax/ajax.searchbyindex.php?q=<PREFIX>`
   - Parses `span.alllink[data-phraseid]`
2. Fetch full occurrences/definitions:
   - `https://apps.ipcc.ch/glossary/ajax/ajax.searchalloccurance.php?q=<PHRASE_ID>&r=`
   - Reads definition from `dd p` (fallback: `dd`)
   - Reads reports from `data-report` attributes
3. Save rows:
   - columns: `id`, `prefix`, `term`, `definition`, `reports`, `downloaded_at`

Politeness/rate-limiting:

- `Sys.sleep(0.3)` between requests
- request timeout set (15s)

Term cleaning:

- Strips suffixes like `<<WGI>>` / guillemet forms from term labels.

## 4. Loading and Cleaning

### 4.1 IPBES loading

- Reads CSV from bundled snapshot.
- Normalizes key columns (concept, definition, deliverables, etc.).
- Splits multi-assessment entries into one row per assessment.
- Cleans HTML/entities with `clean_html()`.

### 4.2 IPCC loading

- Uses cache file first, then bundled snapshot.
- Ensures required columns exist.
- Cleans definitions with `clean_html()`.
- Expands semicolon-separated report list into one row per report for details.

## 5. Merging Logic

Merge is a full outer merge over normalized term keys:

1. Exact key match: `normalise_term(ipbes_concept)` vs `normalise_term(ipcc_term)`
2. Fallback for unmatched IPBES term:
   - removes parenthetical qualifier, e.g. `abundance (ecological)` -> `abundance`
3. Remaining unmatched rows are kept as IPBES-only or IPCC-only entries.

Output includes:

- term labels and counts (`ipbes_n_assessments`, `ipcc_n_reports`)
- detailed list-columns (`ipbes_data`, `ipcc_data`)
- similarity metrics (`sim_within_ipbes`, `sim_within_ipcc`, `sim_between_all`)

## 6. Similarity Calculation (Method)

All similarity scores are text-based cosine similarity on term-frequency vectors
(no external API, no Wikipedia).

### 6.1 Tokenization

For each definition text:

- lower-case
- remove non-letters (`[^a-z ]`)
- split on whitespace
- remove stopwords (internal `STOPWORDS` list)
- no stemming/lemmatization (for example, `impact` and `impacts` are treated as different tokens)
- keep term frequencies

### 6.2 Pair similarity

For two token-frequency vectors `a` and `b`:

- dot product over shared tokens
- cosine:
  - `sim(a,b) = dot(a,b) / (||a|| * ||b||)`
- range:
  - `0` if no shared tokens
  - `NA` if an input is empty after cleaning

### 6.3 Three displayed metrics

1. Within IPBES:
   - mean of all pairwise similarities among IPBES definitions of a term
   - `NA` if fewer than 2 usable IPBES definitions
2. Within IPCC:
   - mean of all pairwise similarities among IPCC definitions of a term
   - `NA` if fewer than 2 usable IPCC definitions
3. Between All Definitions:
   - mean of all cross pairs `IPBES x IPCC`
   - `NA` if one side has no usable definitions

Important detail:

- Duplicates are intentionally kept in the similarity inputs, so repeated
  assessment/report definitions contribute to the averages.

### 6.4 How to interpret the similarity percentages

- Similarity values are lexical (token-overlap based), not semantic/paraphrase
  understanding.
- `100%` means the cleaned token-frequency representation is identical.
- High values generally indicate very similar wording.
- Mid values often indicate partial overlap in terminology.
- Low values indicate mostly different wording/vocabulary.
- These measures are best used for ranking/comparison, not as absolute proof of
  conceptual equivalence.

## 7. Word-by-Word Difference (LCS Diff)

Word-level diff uses Longest Common Subsequence (LCS) over word tokens.

For a compared pair `(text_a, text_b)`:

- unchanged words -> `same`
- words only in `text_a` -> `del` (rendered as `<del>`)
- words only in `text_b` -> `ins` (rendered as `<ins>`)

Interpretation:

- "Deleted" means present only on the left/first text (`text_a`).
- "Inserted" (added) means present only on the right/second text (`text_b`).

In matrix cells, labels clarify orientation:

- `label_a` is the row/first item
- `label_b` is the column/second item

So "added" is always relative to the first (left-labeled) text.

## 8. Overview vs Expanded Detail

### 8.1 Overview table

- One row per merged term.
- Column order:
  - Term
  - Between similarity
  - IPBES definition column (includes within-IPBES similarity bar)
  - IPCC definition column (includes within-IPCC similarity bar)
- Definitions with identical text are grouped; associated
  assessments/reports are shown together (`;` separated, bold).

### 8.2 Expanded row

- Shows a single pairwise similarity matrix over grouped definitions.
- Axis order:
  - all grouped IPBES definitions first
  - then grouped IPCC definitions
- Upper triangle contains scores + word diff blocks.
- Lower triangle is empty by design (to avoid duplicate pair display).
- Very low-similarity pairs can show "Too different" based on threshold.

## 9. Startup and Caching Behavior

Startup data load order:

1. packaged merged cache (`inst/extdata/merged_glossary_cache.rds`) if valid
2. user startup cache (`startup_merged_cache.rds`) if source signatures match
3. full rebuild (load IPBES, load IPCC, merge, compute metrics)

After load, precomputed table HTML/cache fields are prepared and cached for
faster rendering.

This is why current startup is much faster than full recompute on every load.

## 10. Directed Hierarchy Graph (Graph Tab)

The app includes a dedicated `Graph` tab that estimates and visualizes a
directed parent -> child hierarchy among matched terms.

### 10.1 Directed subsumption score

For candidate edge `(parent, child)`, the score is:

- `score = 0.55 * lex_sub + 0.25 * def_contain + 0.20 * def_sim`

Where:

- `lex_sub`: fraction of parent term tokens also present in the child term
- `def_contain`: fraction of parent term tokens present in the child definition text
- `def_sim`: cosine similarity between combined parent/child definition texts

Candidate gates:

- `parent_token_n < child_token_n`
- must pass lexical or definition-containment gate before final scoring

Default display threshold in the graph UI is `min_score = 0.65`.

Interpretation guidance:

- Higher SS scores indicate stronger evidence that the parent term subsumes the
  child term.
- The score combines:
  - lexical containment in term names (`lex_sub`)
  - parent-token containment in child definitions (`def_contain`)
  - overall definition similarity (`def_sim`)
- Raising `Minimum subsumption score` keeps only stronger, more conservative
  edges; lowering it adds weaker/less certain edges.

### 10.2 Graph interaction model

- Click a node to select its connected tree (ancestors + descendants).
- Selected tree remains colored; non-linked nodes/edges are greyed.
- Click background to clear selection.
- Use `Focus Previous Tree` / `Focus Next Tree` to navigate top-level trees.
- Use `Reset View` to fit/pan/zoom back to the currently selected tree.
- Top-level trees are arranged left-to-right by descending tree size (ties:
  alphabetical by root term).
- Root labels are rendered above root nodes with stronger visual emphasis.

All edges above the minimum score threshold are shown (no max-edges truncation).

Selection is shared with the main table: selected graph terms are highlighted in
the table rows.

### 10.3 Graph controls and tooltips

The graph controls include inline hover tooltips (`title` attributes) that
explain each control:

- `Select Tree`: choose the currently plotted tree (type-ahead filtering).
- `Sort trees alphabetically`: toggle selector ordering (alphabetical vs
  descending node count).
- `Focus Previous Tree` / `Focus Next Tree`: move through trees following the
  selector ordering.
- `Reset View`: reset pan/zoom to fit the selected tree.
- `Minimum subsumption score`: filter edges to `score >= threshold`.
  Higher values keep only stronger parent -> child relations.
- `Keep only best parent per child`: at current threshold, retain only the
  strongest incoming edge for each child term.
- `Export format`: choose `HTML (offline)` or `PDF (print)`.
- `Export`: generate a report with current settings, a graph snapshot, the
  selected-tree directed-edge table, and the selected-tree glossary table.
  In HTML exports, the graph is interactive (zoom, pan, node dragging, and
  hover tooltips).

### 10.4 Graph caching

Hierarchy edge scoring is cached in:

- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/hierarchy_edges_cache.rds`

If runtime cache is absent (for example on hosted cold starts), the app falls
back to bundled cache:

- `inst/extdata/hierarchy_edges_cache.rds`

Cache is invalidated by a fingerprint over merged term/definition content.

## 11. Local vs Hosted Behavior

By default (comparison app):

- Local run: live IPCC update button enabled
- shinyapps.io hosted runtime: live update disabled ("Hosted mode" label shown)

Override via env var:

- `GLOSSARY_ENABLE_LIVE_UPDATE=1` -> force enable
- `GLOSSARY_ENABLE_LIVE_UPDATE=0` -> force disable

Hosted-safe deploy script (`scripts/deploy_shinyapps_compare.R`) sets hosted default
to disabled unless explicitly overridden.

## 12. Running Locally

### 11.1 Run apps

```r
# Comparison app
glossary.ipbes.ipcc::run_app()

# Glossary explorer app
glossary.ipbes.ipcc::run_glossary()
```

### 11.2 Run comparison app and force update behavior

```r
# disable update button
glossary.ipbes.ipcc::run_app(enable_live_update = FALSE)

# or via env var
Sys.setenv(GLOSSARY_ENABLE_LIVE_UPDATE = "0")
glossary.ipbes.ipcc::run_app()
```

### 11.3 Deploy entrypoints for shinyapps.io

- Comparison app:
  - entrypoint: `app_compare.R`
  - deploy script: `scripts/deploy_shinyapps_compare.R`
- Glossary explorer app:
  - entrypoint: `app_glossary.R`
  - deploy script: `scripts/deploy_shinyapps_glossary.R`

### 11.4 Refresh packaged snapshots for release

For package-maintained snapshots (not just runtime cache):

```r
source("data-raw/prepare_data.R")
```

This regenerates:

- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/merged_glossary_cache.rds`
- `inst/extdata/hierarchy_edges_cache.rds`

If you already updated one or both CSV files in `inst/extdata/` and only need
to refresh bundled caches, run:

```r
system("Rscript inst/scripts/update_bundled_caches.R")
```

Use `--force` to rebuild even when metadata indicates caches are current.

To scrape a fresh IPCC snapshot and then refresh all bundled caches in one step:

```r
system("Rscript inst/scripts/scrape_ipcc_and_update_caches.R")
```

Then rebuild/reinstall package as needed.

### 11.5 Glossary explorer UX notes (v0.9.8)

Recent glossary explorer presentation updates include:

- dedicated in-app `About` modal with maintained project context text
- footer placement of `GitHub Issues` under the data-source links
- dynamic section headers in the form
  `"<selected term> in IPBES Glossary"` / `"<selected term> in IPCC Glossary"`
- combined `See also` list across displayed definitions, sorted alphabetically
- attribution wording updated to
  `Senckenberg Biodiversity and Climate`
