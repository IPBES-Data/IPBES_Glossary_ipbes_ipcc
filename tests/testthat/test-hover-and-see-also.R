test_that("hover text names the source and quotes the definition", {
  merged <- make_merged()
  row <- row_for(merged, "biodiversity")

  ipbes <- .glossary_hover_text_from_row(row, "ipbes")
  expect_match(ipbes, "^IPBES - ")
  expect_match(ipbes, "variability", fixed = TRUE)

  ipcc <- .glossary_hover_text_from_row(row, "ipcc")
  expect_match(ipcc, "^IPCC - ")
})

test_that("hover text reports absence when a source has no definition", {
  merged <- make_merged()

  expect_equal(.glossary_hover_text_from_row(row_for(merged, "pollination"), "ipcc"),
               "IPCC: no definition available.")
  expect_equal(.glossary_hover_text_from_row(row_for(merged, "attribution"), "ipbes"),
               "IPBES: no definition available.")
})

test_that("hover lookup covers every catalog term and combines both sources", {
  merged <- make_merged()
  hover <- .glossary_hover_lookup(merged, "both")

  expect_setequal(names(hover), .glossary_term_catalog(merged, "both"))
  expect_match(hover[["biodiversity"]], "IPBES - ")
  expect_match(hover[["biodiversity"]], "IPCC - ")
})

test_that("see-also collects linked terms per source", {
  merged <- make_merged()
  dict <- .glossary_highlight_dictionary(.glossary_term_catalog(merged, "both"))
  links <- .glossary_collect_link_terms(row_for(merged, "ecosystem services"), "both", dict)

  expect_true("pollination" %in% links$all)
  expect_true("pollination" %in% links$ipbes)
})

test_that("see-also is empty when a definition links nothing", {
  merged <- make_merged()
  dict <- .glossary_highlight_dictionary(.glossary_term_catalog(merged, "both"))
  links <- .glossary_collect_link_terms(
    row_for(merged, "earth system feedbacks"), "both", dict
  )

  expect_equal(links$all, character(0))
})

test_that("IPCC pointers do not feed the derived See also panel", {
  merged <- make_merged()
  dict <- .glossary_highlight_dictionary(.glossary_term_catalog(merged, "both"))
  links <- .glossary_collect_link_terms(row_for(merged, "attribution"), "both", dict)

  # The See also panel is derived across both sources from terms occurring in
  # definition TEXT. "Attribution" has no definition, only a pointer, so it
  # contributes nothing here -- the pointer is rendered in its own card block
  # instead. Keeping these separate is deliberate: the panel is cross-source,
  # pointers are IPCC-specific.
  expect_equal(links$all, character(0))
})

test_that("see-also mode restricts which sources contribute", {
  merged <- make_merged()
  dict <- .glossary_highlight_dictionary(.glossary_term_catalog(merged, "both"))
  row <- row_for(merged, "ecosystem services")

  expect_equal(.glossary_collect_link_terms(row, "ipbes", dict)$ipcc, character(0))
  expect_equal(.glossary_collect_link_terms(row, "ipcc", dict)$ipbes, character(0))
})

test_that("see-also UI renders links, and a placeholder when there are none", {
  html <- as.character(.glossary_see_also_ui(list(all = c("b term", "a term"))))
  expect_match(html, "a term", fixed = TRUE)
  expect_match(html, "glossary-see-link", fixed = TRUE)
  # sorted alphabetically
  expect_lt(regexpr("a term", html, fixed = TRUE),
            regexpr("b term", html, fixed = TRUE))

  empty <- as.character(.glossary_see_also_ui(list(all = character(0))))
  expect_match(empty, "No linked glossary terms", fixed = TRUE)
})
