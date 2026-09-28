# Cover for IPCC report entries that carry no definition of their own, and for
# the pointer block that is rendered apart from the definition.
#
# A report entry may have a definition, related-term pointers, both, or
# neither. Pointers (See / See Also / Sub-terms) are IPCC's own editorial data
# and are shown in their own block inside the card -- never spliced into the
# quoted definition, and never merged into the derived See also panel.

test_that("every catalog term resolves to a row on its own side", {
  merged <- make_merged()

  for (mode in c("both", "ipbes", "ipcc")) {
    terms <- .glossary_term_catalog(merged, mode)
    unresolved <- terms[vapply(terms, function(tm) {
      is.null(.glossary_find_row(merged, tm, mode))
    }, logical(1))]
    expect_equal(unresolved, character(0), info = paste("mode:", mode))
  }
})

test_that("a pointer-only entry shows the pointer instead of a dead end", {
  merged <- make_merged(highlight = TRUE)
  row <- row_for(merged, "attribution")

  html <- as.character(.glossary_source_section_ui(
    row = row, source = "ipcc", dict = NULL, hover_lookup = NULL,
    term_label = "attribution"
  ))

  expect_false(grepl("No definitions available.", html, fixed = TRUE))
  expect_match(html, "glossary-def-xref", fixed = TRUE)
  expect_match(html, "See: ", fixed = TRUE)
  expect_match(html, ">Biodiversity</a>", fixed = TRUE)
})

test_that("a pointer-only entry is labelled 'Listed in', not 'As defined in'", {
  merged <- make_merged(highlight = TRUE)
  html <- as.character(.glossary_source_section_ui(
    row = row_for(merged, "attribution"), source = "ipcc",
    dict = NULL, hover_lookup = NULL, term_label = "attribution"
  ))

  expect_match(html, "Listed in:", fixed = TRUE)
})

test_that("the pointer target is rendered as a clickable link", {
  merged <- make_merged(highlight = TRUE)
  detail <- row_for(merged, "attribution")$ipcc_data[[1]]
  html <- detail$xref_html[detail$report == "SRCCL"]

  expect_match(html, "glossary-term-link", fixed = TRUE)
  expect_match(html, 'data-term="biodiversity"', fixed = TRUE)
})

test_that("a definition and a pointer are rendered as separate elements", {
  merged <- make_merged(highlight = TRUE)
  html <- as.character(.glossary_source_section_ui(
    row = row_for(merged, "biodiversity"), source = "ipcc",
    dict = NULL, hover_lookup = NULL, term_label = "biodiversity"
  ))

  # AR6 has both. The definition sits in the body, the pointer in its own
  # block, and the pointer text is never inside the quoted definition.
  expect_match(html, "glossary-def-body", fixed = TRUE)
  expect_match(html, "glossary-def-xref", fixed = TRUE)
  expect_match(html, "See also: ", fixed = TRUE)
  expect_match(html, "As defined in:", fixed = TRUE)

  body <- sub('.*glossary-def-body[^>]*>(.*?)glossary-def-xref.*', "\\1", html)
  expect_false(grepl("See also:", body, fixed = TRUE))
})

test_that("an entry with neither definition nor pointer stays empty", {
  merged <- make_merged(highlight = TRUE)
  html <- as.character(.glossary_source_section_ui(
    row = row_for(merged, "earth system feedbacks"), source = "ipcc",
    dict = NULL, hover_lookup = NULL, term_label = "earth system feedbacks"
  ))

  expect_match(html, "No definitions available.", fixed = TRUE)
})

test_that("reports sharing a definition but differing pointers stay separate", {
  detail <- data.frame(
    report = c("AR6", "SR15", "SRCCL"),
    definition = rep("The same definition text.", 3),
    xref = c("See also: Alpha", "See also: Alpha", "See also: Beta"),
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  # AR6 and SR15 merge; SRCCL points elsewhere and must not be folded in.
  expect_equal(nrow(grouped), 2L)
  expect_equal(grouped$report[[1]], "AR6\nSR15")
  expect_equal(grouped$report[[2]], "SRCCL")
})

test_that("grouping keeps a row that has pointers but no definition", {
  detail <- data.frame(
    report = "SRCCL",
    definition = "",
    xref = "See: Something",
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  expect_equal(nrow(grouped), 1L)
  expect_equal(grouped$definition, "")
  expect_equal(grouped$xref, "See: Something")
})

test_that("grouping still drops rows with neither definition nor pointers", {
  detail <- data.frame(
    report = c("AR6", "SR15"),
    definition = c("", "Real text."),
    xref = c("", ""),
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  expect_equal(nrow(grouped), 1L)
  expect_equal(grouped$definition, "Real text.")
})

test_that("report attributions survive regardless of definition text", {
  dir <- withr::local_tempdir()
  ipcc <- summarise_ipcc(load_ipcc(
    cache_dir = dir, bundled_path = write_ipcc_csv(dir, default_ipcc_rows())
  ))
  attribution <- ipcc[ipcc$term == "Attribution", ]

  expect_equal(attribution$n_reports, 2L)
  expect_setequal(attribution$ipcc_data[[1]]$report, c("SRCCL", "AR6"))
})
