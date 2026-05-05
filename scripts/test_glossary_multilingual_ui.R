source(file.path("R", "utils.R"))
source(file.path("R", "data_multilingual.R"))
source(file.path("R", "ipcc_report_names.R"))
source(file.path("R", "app_glossary.R"))

expect <- function(ok, msg) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", msg))
  cat(sprintf("OK: %s\n", msg))
}

runtime <- load_unified_multilingual_runtime(file.path("inst", "extdata", "glossary_multilingual_runtime.rds"))
expect(!is.null(runtime) && nrow(runtime) > 0, "runtime multilingual cache loads")

ml <- .glossary_prepare_multilingual(runtime, merged = NULL)

catalog_en <- .glossary_build_catalog(ml, mode = "both", main_language = "en")
catalog_fr <- .glossary_build_catalog(ml, mode = "both", main_language = "fr")

expect(nrow(catalog_en) > 0, "english catalog is populated")
expect(nrow(catalog_fr) > 0, "french catalog is populated")
expect(any(catalog_en$source == "IPBES"), "english catalog includes IPBES")
expect(!any(catalog_fr$source == "IPBES"), "non-english catalog hides IPBES")
expect(all(grepl("^(IPBES|IPCC)::", catalog_en$value)), "catalog values use source::concept key format")
expect(length(unique(catalog_en$value)) == nrow(catalog_en), "catalog keys are unique")

# Ensure section renderer always includes all language cards with placeholders when needed.
ipbes_row <- catalog_en[catalog_en$source == "IPBES", , drop = FALSE][1, , drop = FALSE]
expect(nrow(ipbes_row) == 1, "found IPBES concept for section rendering test")

dict_en <- .glossary_highlight_dictionary_map(ml, catalog_en, mode = "both")
hover_en <- .glossary_hover_lookup_from_catalog(ml, catalog_en, mode = "both")

ipbes_section <- .glossary_multilingual_source_section_ui(
  multilingual = ml,
  source = "IPBES",
  concept_id = ipbes_row$concept_id[[1]],
  languages = .glossary_languages(),
  dict = dict_en,
  hover_lookup = hover_en,
  main_language = "en"
)

ipbes_html <- paste(as.character(ipbes_section$ui), collapse = "")
expect(length(gregexpr("glossary-lang-card", ipbes_html, fixed = TRUE)[[1]]) >= 1, "IPBES section renders language cards")
expect(!grepl(">NA<", ipbes_html, fixed = TRUE), "IPBES section does not show literal NA term labels")

# See-also links resolve to internal concept keys.
ipcc_row <- catalog_fr[catalog_fr$source == "IPCC", , drop = FALSE][1, , drop = FALSE]
expect(nrow(ipcc_row) == 1, "found IPCC concept for see-also test")

dict_fr <- .glossary_highlight_dictionary_map(ml, catalog_fr, mode = "ipcc")
hover_fr <- .glossary_hover_lookup_from_catalog(ml, catalog_fr, mode = "ipcc")
ipcc_section <- .glossary_multilingual_source_section_ui(
  multilingual = ml,
  source = "IPCC",
  concept_id = ipcc_row$concept_id[[1]],
  languages = .glossary_languages(),
  dict = dict_fr,
  hover_lookup = hover_fr,
  main_language = "fr"
)

links <- .glossary_collect_link_keys(
  source_sections = list(ipbes = NULL, ipcc = ipcc_section$data),
  mode = "ipcc",
  dict = dict_fr
)

if (length(links$all) > 0) {
  expect(all(links$all %in% catalog_fr$value), "see-also links map to selectable concept keys")
  see_ui <- .glossary_see_also_ui_multilingual(links, catalog_fr, ml, "fr")
  see_html <- paste(as.character(see_ui), collapse = "")
  expect(grepl("\\(", see_html), "see-also labels include multilingual parenthetical terms")
} else {
  cat("OK: see-also links absent for sampled term; key mapping check skipped.\n")
}

cat("All glossary multilingual UI checks passed.\n")
