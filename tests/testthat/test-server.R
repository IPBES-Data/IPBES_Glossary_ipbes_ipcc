# End-to-end behaviour of the explorer's server logic, driven through
# shiny::testServer so term selection, navigation and source switching are
# exercised as the app actually runs them.

# renderUI results arrive as a list with an `html` element.
render_html <- function(out) {
  if (is.null(out)) return("")
  paste(as.character(out$html), collapse = "")
}

test_that("no definition view is rendered until a term is selected", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    expect_null(output$glossary_definition_view)
  })
})

test_that("selecting a term renders both source sections", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    session$setInputs(term = "biodiversity")

    html <- render_html(output$glossary_definition_view)
    expect_match(html, "in IPBES Glossary", fixed = TRUE)
    expect_match(html, "in IPCC Glossary", fixed = TRUE)
    expect_match(html, "As defined in", fixed = TRUE)
  })
})

test_that("clicking a term inside a definition navigates to it", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    session$setInputs(term = "ecosystem services")
    session$setInputs(term_click = "pollination")

    expect_equal(selected_term_r(), "pollination")
    expect_match(render_html(output$glossary_definition_view),
                 "transfer of pollen", fixed = TRUE)
  })
})

test_that("an unknown clicked term is ignored rather than clearing the view", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    session$setInputs(term = "biodiversity")
    session$setInputs(term_click = "not a glossary term")

    expect_equal(selected_term_r(), "biodiversity")
  })
})

test_that("switching source narrows the view but keeps the selected term", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    session$setInputs(term = "biodiversity")

    session$setInputs(source_mode = "ipcc")
    expect_equal(selected_term_r(), "biodiversity")
    html <- render_html(output$glossary_definition_view)
    expect_match(html, "in IPCC Glossary", fixed = TRUE)
    expect_false(grepl("in IPBES Glossary", html, fixed = TRUE))

    session$setInputs(source_mode = "ipbes")
    html <- render_html(output$glossary_definition_view)
    expect_match(html, "in IPBES Glossary", fixed = TRUE)
    expect_false(grepl("in IPCC Glossary", html, fixed = TRUE))
  })
})

test_that("a term absent from the new source is dropped on switching", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "ipbes")
    session$setInputs(term = "pollination")
    expect_equal(selected_term_r(), "pollination")

    session$setInputs(source_mode = "ipcc")
    expect_equal(selected_term_r(), "")
    expect_null(output$glossary_definition_view)
  })
})

test_that("an invalid source mode falls back to both", {
  merged <- make_merged(highlight = TRUE)
  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "nonsense")
    expect_equal(mode_r(), "both")
  })
})
