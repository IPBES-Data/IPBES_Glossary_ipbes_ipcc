# Smoke test for the real startup path. This constructs the app the way
# run_glossary() does, which is the only test that exercises
# .create_glossary_app() -> .load_merged_data() against the bundled data.
#
# A fresh temp cache_dir means no user-cached ipcc_glossary.csv shadows the
# bundled snapshot, so the packaged merged cache is used and this stays fast.

test_that("the app object builds from the bundled data", {
  skip_on_cran()
  skip_if_not(nzchar(.pkg_file("extdata", "merged_glossary_cache.rds")),
              "packaged cache not available")

  cache_dir <- withr::local_tempdir()
  app <- suppressMessages(.create_glossary_app(cache_dir = cache_dir))

  expect_s3_class(app, "shiny.appobj")
})

test_that("the bundled data reaches the server with its highlight cache intact", {
  skip_on_cran()
  skip_if_not(nzchar(.pkg_file("extdata", "merged_glossary_cache.rds")),
              "packaged cache not available")

  cache_dir <- withr::local_tempdir()
  merged <- suppressMessages(
    .load_merged_data(cache_dir, prepare_highlight_cache = TRUE)
  )

  expect_gt(nrow(merged), 1000L)
  expect_true(.has_current_highlight_cache(merged))

  shiny::testServer(.build_glossary_server(merged), {
    session$setInputs(source_mode = "both")
    session$setInputs(term = "biodiversity")

    expect_equal(selected_term_r(), "biodiversity")
    html <- paste(as.character(output$glossary_definition_view$html), collapse = "")
    expect_match(html, "in IPBES Glossary", fixed = TRUE)
    expect_match(html, "glossary-term-link", fixed = TRUE)
  })
})

test_that("terms whose first word carries punctuation are linked in real data", {
  skip_on_cran()
  skip_if_not(nzchar(.pkg_file("extdata", "merged_glossary_cache.rds")),
              "packaged cache not available")

  merged <- .load_packaged_merged_cache()
  dict <- .glossary_highlight_dictionary(.glossary_term_catalog(merged, "both"))

  for (term in c("agro-ecological zone", "asia-pacific region")) {
    html <- .glossary_highlight_definition(
      paste("A study of", term, "in practice."), dict, NULL
    )
    expect_match(html, paste0('data-term="', term, '"'), fixed = TRUE, info = term)
  }
})
