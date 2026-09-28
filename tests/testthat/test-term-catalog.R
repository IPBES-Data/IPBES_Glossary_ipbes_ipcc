test_that("term catalog is lowercased, de-duplicated and sorted", {
  merged <- make_merged()
  terms <- .glossary_term_catalog(merged, "both")

  expect_equal(terms, sort(terms))
  expect_false(any(duplicated(terms)))
  expect_true(all(terms == tolower(terms)))
})

test_that("term catalog respects the selected source", {
  merged <- make_merged()

  expect_false("attribution" %in% .glossary_term_catalog(merged, "ipbes"))
  expect_true("pollination" %in% .glossary_term_catalog(merged, "ipbes"))

  expect_true("attribution" %in% .glossary_term_catalog(merged, "ipcc"))
  expect_false("pollination" %in% .glossary_term_catalog(merged, "ipcc"))

  both <- .glossary_term_catalog(merged, "both")
  expect_true(all(c("attribution", "pollination") %in% both))
})

test_that("term catalog is empty for empty or NULL data", {
  expect_equal(.glossary_term_catalog(NULL), character(0))
  expect_equal(.glossary_term_catalog(make_merged()[0, ], "both"), character(0))
})

test_that("find_row matches case-insensitively and via normalisation", {
  merged <- make_merged()

  expect_equal(row_for(merged, "biodiversity")$matched_term, "biodiversity")
  expect_equal(row_for(merged, "BIODIVERSITY")$matched_term, "biodiversity")
  expect_equal(row_for(merged, "attribution")$matched_term, "Attribution")
})

test_that("find_row honours the source mode", {
  merged <- make_merged()

  expect_null(row_for(merged, "pollination", "ipcc"))
  expect_null(row_for(merged, "attribution", "ipbes"))
  expect_false(is.null(row_for(merged, "attribution", "ipcc")))
})

test_that("find_row returns NULL for unknown or empty terms", {
  merged <- make_merged()

  expect_null(row_for(merged, "not a glossary term"))
  expect_null(row_for(merged, ""))
  expect_null(.glossary_find_row(NULL, "biodiversity"))
})

test_that("resolve_choice prefers an exact match then a normalised one", {
  choices <- c("biodiversity", "ecosystem services")

  expect_equal(.glossary_resolve_choice("biodiversity", choices), "biodiversity")
  expect_equal(.glossary_resolve_choice("Ecosystem Services", choices), "ecosystem services")
  expect_equal(.glossary_resolve_choice("unknown", choices), "")
  expect_equal(.glossary_resolve_choice(NULL, choices), "")
  expect_equal(.glossary_resolve_choice("biodiversity", character(0)), "")
})
