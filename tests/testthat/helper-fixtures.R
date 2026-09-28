# Shared fixtures for the glossary explorer tests.
# =============================================================================
# These build small, synthetic glossaries and push them through the real
# load -> summarise -> merge pipeline, so the fixtures exercise the same code
# path the app uses at startup rather than a hand-built imitation of it.

# Write a minimal IPBES source CSV and return its path.
write_ipbes_csv <- function(dir, rows) {
  path <- file.path(dir, "ipbes_glossary.csv")
  utils::write.csv(rows, path, row.names = FALSE, fileEncoding = "UTF-8")
  path
}

# Write a minimal IPCC source CSV and return its path.
write_ipcc_csv <- function(dir, rows) {
  path <- file.path(dir, "ipcc_glossary.csv")
  utils::write.csv(rows, path, row.names = FALSE, fileEncoding = "UTF-8")
  path
}

default_ipbes_rows <- function() {
  data.frame(
    Concept = c(
      "Biodiversity",
      "biodiversity",
      "Ecosystem services",
      "Abundance (ecological)",
      "Pollination"
    ),
    Definition = c(
      "The variability among living organisms including diversity within species.",
      "Variability among living organisms from all sources.",
      "The benefits people obtain from ecosystems, including pollination.",
      "The total number of individuals of a taxon in an area.",
      "The transfer of pollen, underpinning ecosystem services."
    ),
    `Deliverable(s)` = c(
      "Global assessment",
      "Pollination assessment",
      "Global assessment, Values assessment",
      "Global assessment",
      "Pollination assessment"
    ),
    term_id = c(1, 1, 2, 3, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

default_ipcc_rows <- function() {
  # One row per (term, report). Report wordings genuinely differ in the real
  # glossary, and some report entries carry no definition at all -- only a
  # "See <other term>" cross-reference, or nothing.
  data.frame(
    id = c("10", "10", "11", "12", "13", "13", "14"),
    prefix = c("B", "B", "E", "A", "A", "A", "E"),
    term = c("Biodiversity", "Biodiversity", "Ecosystem services",
             "Abundance", "Attribution", "Attribution",
             "Earth system feedbacks"),
    report = c("AR6", "SR15", "AR6", "AR5-WG1", "SRCCL", "AR6", "SRCCL"),
    definition = c(
      "Biodiversity means the variability among living organisms.",
      "The variability among living organisms, including within species.",
      "Ecological processes having value to society, such as pollination.",
      "The number of individuals present.",
      "",   # pointer only
      "",   # neither definition nor pointer
      ""    # neither definition nor pointer
    ),
    # A definition and a pointer can coexist: "Abundance" in AR5-WG1 is defined
    # and still points elsewhere, as happens upstream.
    xref_kind = c("see_also", NA, NA, "sub_terms", "see", NA, NA),
    xref_target = c("Pollination", NA, NA, "Abundance (ecological)",
                    "Biodiversity", NA, NA),
    xref_target_id = c("4", NA, NA, "3", "10", NA, NA),
    downloaded_at = rep("2026-09-15", 7),
    stringsAsFactors = FALSE
  )
}

# The legacy one-row-per-term shape, for back-compatibility tests.
legacy_ipcc_rows <- function() {
  data.frame(
    id = c("10", "11"),
    prefix = c("B", "E"),
    term = c("Biodiversity", "Ecosystem services"),
    definition = c("Biodiversity means the variability among living organisms.",
                   "Ecological processes having value to society."),
    reports = c("AR6; SR15", "AR6"),
    downloaded_at = rep("2026-05-06", 2),
    stringsAsFactors = FALSE
  )
}

# Build a merged glossary tibble from the default (or supplied) fixtures.
make_merged <- function(ipbes_rows = default_ipbes_rows(),
                        ipcc_rows = default_ipcc_rows(),
                        highlight = FALSE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  ipbes <- summarise_ipbes(load_ipbes(write_ipbes_csv(dir, ipbes_rows)))
  ipcc <- summarise_ipcc(load_ipcc(
    cache_dir = dir,
    bundled_path = write_ipcc_csv(dir, ipcc_rows)
  ))
  merged <- merge_glossaries(ipbes, ipcc)
  if (isTRUE(highlight)) {
    merged <- suppressMessages(.prepare_glossary_highlight_data(merged))
  }
  merged
}

# Row lookup helper that does not depend on row ordering.
row_for <- function(merged, term, mode = "both") {
  .glossary_find_row(merged, term, mode)
}
