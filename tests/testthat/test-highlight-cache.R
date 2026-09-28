test_that("a freshly merged table has no highlight cache", {
  expect_false(.has_current_highlight_cache(make_merged()))
})

test_that("preparing the highlight cache stamps the current version", {
  prepared <- make_merged(highlight = TRUE)

  expect_true(.has_current_highlight_cache(prepared))
  expect_true(all(prepared$highlight_cache_version == .HIGHLIGHT_CACHE_VERSION))
})

test_that("a stale cache version is rejected so the cache is rebuilt", {
  prepared <- make_merged(highlight = TRUE)
  prepared$highlight_cache_version <- .HIGHLIGHT_CACHE_VERSION + 1L

  expect_false(.has_current_highlight_cache(prepared))
})

test_that("preparing the cache pre-renders definition_html and see_also_list", {
  prepared <- make_merged(highlight = TRUE)
  row <- row_for(prepared, "ecosystem services")

  expect_true("definition_html" %in% names(row$ipbes_data[[1]]))
  expect_match(row$ipbes_data[[1]]$definition_html[[1]],
               "glossary-term-link", fixed = TRUE)
  expect_true("see_also_list" %in% names(prepared))
  expect_true("pollination" %in% row$see_also_list[[1]]$all)
})

test_that("preparing an already-prepared table is a no-op", {
  prepared <- make_merged(highlight = TRUE)
  expect_identical(.prepare_glossary_highlight_data(prepared), prepared)
})

test_that("an empty table is treated as already cached", {
  expect_true(.has_current_highlight_cache(make_merged()[0, ]))
})
