test_that("merge_glossaries performs a full outer join", {
  merged <- make_merged()

  # biodiversity + ecosystem services match on both sides; abundance matches
  # via the qualifier-stripped pass; pollination is IPBES-only; Attribution
  # and Earth system feedbacks are IPCC-only.
  expect_equal(nrow(merged), 6L)
  expect_setequal(
    merged$matched_term,
    c("biodiversity", "ecosystem services", "abundance (ecological)",
      "pollination", "Attribution", "Earth system feedbacks")
  )
})

test_that("merge_glossaries matches on the qualifier-stripped term (pass 2)", {
  merged <- make_merged()
  row <- merged[merged$matched_term == "abundance (ecological)", ]

  expect_equal(row$ipbes_concept, "abundance (ecological)")
  expect_equal(row$ipcc_term, "Abundance")
})

test_that("unmatched rows keep NA on the absent side but retain their own data", {
  merged <- make_merged()

  ipbes_only <- merged[merged$matched_term == "pollination", ]
  expect_true(is.na(ipbes_only$ipcc_term))
  expect_equal(nrow(ipbes_only$ipcc_data[[1]]), 0L)
  expect_gt(nrow(ipbes_only$ipbes_data[[1]]), 0L)

  ipcc_only <- merged[merged$matched_term == "Attribution", ]
  expect_true(is.na(ipcc_only$ipbes_concept))
  expect_equal(nrow(ipcc_only$ipbes_data[[1]]), 0L)
})

test_that("merge_glossaries never matches one IPCC term to two IPBES concepts", {
  merged <- make_merged()
  ipcc_terms <- merged$ipcc_term[!is.na(merged$ipcc_term)]

  expect_false(any(duplicated(ipcc_terms)))
})

test_that("merge_glossaries tolerates an empty IPCC glossary", {
  dir <- withr::local_tempdir()
  ipbes <- summarise_ipbes(load_ipbes(write_ipbes_csv(dir, default_ipbes_rows())))
  empty <- summarise_ipcc(.empty_ipcc_tibble())

  merged <- merge_glossaries(ipbes, empty)
  expect_equal(nrow(merged), 4L)
  expect_true(all(is.na(merged$ipcc_term)))
})

test_that("merged output is sorted by term and carries both detail list-columns", {
  merged <- make_merged()

  expect_equal(merged$matched_term, sort(merged$matched_term))
  expect_true(is.list(merged$ipbes_data))
  expect_true(is.list(merged$ipcc_data))
})
