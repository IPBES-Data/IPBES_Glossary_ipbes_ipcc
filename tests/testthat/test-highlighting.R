mk_dict <- function(terms) .glossary_highlight_dictionary(terms)
no_hover <- function() NULL

test_that("dictionary orders terms longest-first so specific terms win", {
  dict <- mk_dict(c("services", "ecosystem services"))
  expect_equal(dict$terms[[1]], "ecosystem services")
})

test_that("dictionary drops blanks and duplicates", {
  dict <- mk_dict(c("a term", "a term", "", NA, "  "))
  expect_equal(dict$terms, "a term")
})

test_that("highlighting links a known term and carries data-term", {
  dict <- mk_dict("biodiversity")
  html <- .glossary_highlight_definition("Loss of biodiversity is severe.", dict, no_hover())

  expect_match(html, "class=\"glossary-term-link\"", fixed = TRUE)
  expect_match(html, "data-term=\"biodiversity\"", fixed = TRUE)
  expect_match(html, ">biodiversity</a>", fixed = TRUE)
})

test_that("the longest matching term wins and matches never overlap", {
  dict <- mk_dict(c("services", "ecosystem services"))
  html <- .glossary_highlight_definition("Valuing ecosystem services today.", dict, no_hover())

  expect_match(html, "data-term=\"ecosystem services\"", fixed = TRUE)
  expect_false(grepl("data-term=\"services\"", html, fixed = TRUE))
  expect_equal(length(gregexpr("<a ", html, fixed = TRUE)[[1]]), 1L)
})

test_that("matching is case-insensitive but preserves the original casing", {
  dict <- mk_dict("biodiversity")
  html <- .glossary_highlight_definition("Biodiversity matters.", dict, no_hover())

  expect_match(html, ">Biodiversity</a>", fixed = TRUE)
  expect_match(html, "data-term=\"biodiversity\"", fixed = TRUE)
})

test_that("terms are only matched on whole-word boundaries", {
  dict <- mk_dict("art")
  html <- .glossary_highlight_definition("A cartographic chart.", dict, no_hover())

  expect_false(grepl("<a ", html, fixed = TRUE))
})

test_that("HTML in definitions is escaped, not emitted as markup", {
  dict <- mk_dict("biodiversity")
  html <- .glossary_highlight_definition("<script>x</script> and biodiversity", dict, no_hover())

  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("terms whose first word contains punctuation are still matched", {
  # Regression: the candidate prefilter compares the term's first word against
  # alphanumeric tokens from the text, so a whitespace-split first word such as
  # "agro-ecological" or "(model)" would never match.
  for (term in c("agro-ecological zone", "(model) ensemble", "asia-pacific region")) {
    dict <- mk_dict(term)
    html <- .glossary_highlight_definition(paste("A study of", term, "here."),
                                           dict, no_hover())
    expect_match(html, "glossary-term-link", fixed = TRUE, info = term)
    expect_match(html, paste0("data-term=\"", term, "\""), fixed = TRUE, info = term)
  }
})

test_that("regex metacharacters in terms are escaped", {
  dict <- mk_dict("CO2-equivalent (CO2-eq)")
  html <- .glossary_highlight_definition("Measured in CO2-equivalent (CO2-eq) units.",
                                         dict, no_hover())

  expect_match(html, "data-term=\"CO2-equivalent (CO2-eq)\"", fixed = TRUE)
})

test_that("empty or unmatched text degrades gracefully", {
  dict <- mk_dict("biodiversity")

  expect_equal(.glossary_highlight_definition("", dict, no_hover()), "—")
  expect_equal(.glossary_highlight_definition(NA_character_, dict, no_hover()), "—")
  expect_equal(.glossary_highlight_definition("nothing here", dict, no_hover()), "nothing here")
})

test_that("an empty dictionary escapes the text and links nothing", {
  empty <- mk_dict(character(0))
  html <- .glossary_highlight_definition("a & b", empty, no_hover())

  expect_equal(html, "a &amp; b")
})

test_that("hover text becomes a newline-encoded title attribute", {
  dict <- mk_dict("biodiversity")
  hover <- c(biodiversity = "IPBES - Global: variety of life.\nIPCC - AR6: variability.")
  html <- .glossary_highlight_definition("On biodiversity.", dict, hover)

  expect_match(html, "&#10;", fixed = TRUE)
  expect_false(grepl("title=\"\"", html, fixed = TRUE))
})

test_that("find_terms_in_text returns matched terms in order of appearance", {
  dict <- mk_dict(c("biodiversity", "pollination"))
  found <- .glossary_find_terms_in_text("Pollination sustains biodiversity.", dict)

  expect_equal(found, c("pollination", "biodiversity"))
})

test_that("find_terms_in_text de-duplicates repeated terms", {
  dict <- mk_dict("biodiversity")
  expect_equal(
    .glossary_find_terms_in_text("biodiversity and more biodiversity", dict),
    "biodiversity"
  )
})

test_that("find_terms_in_text handles empty input", {
  dict <- mk_dict("biodiversity")

  expect_equal(.glossary_find_terms_in_text("", dict), character(0))
  expect_equal(.glossary_find_terms_in_text(NA_character_, dict), character(0))
  expect_equal(.glossary_find_terms_in_text("text", mk_dict(character(0))), character(0))
})
