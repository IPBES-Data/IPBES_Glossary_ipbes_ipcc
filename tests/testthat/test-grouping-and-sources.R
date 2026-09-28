test_that("identical definitions from different sources are grouped into one row", {
  detail <- data.frame(
    report = c("AR6", "SR15", "AR5-WG1"),
    definition = c("Same text.", "Same text.", "Different text."),
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  expect_equal(nrow(grouped), 2L)
  expect_equal(grouped$report[[1]], "AR6\nSR15")
  expect_equal(grouped$definition[[2]], "Different text.")
})

test_that("grouping drops rows with blank definitions", {
  detail <- data.frame(
    report = c("AR6", "SR15"),
    definition = c("", "Real text."),
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  expect_equal(nrow(grouped), 1L)
  expect_equal(grouped$definition, "Real text.")
})

test_that("grouping returns a correctly named empty frame for empty input", {
  grouped <- .glossary_group_definitions(
    data.frame(report = character(0), definition = character(0)), "report"
  )

  expect_equal(nrow(grouped), 0L)
  expect_equal(names(grouped), c("report", "definition"))
  expect_equal(nrow(.glossary_group_definitions(NULL, "report")), 0L)
})

test_that("grouping preserves pre-computed definition_html when present", {
  detail <- data.frame(
    report = "AR6",
    definition = "Text.",
    definition_html = "<a>Text.</a>",
    stringsAsFactors = FALSE
  )
  grouped <- .glossary_group_definitions(detail, "report")

  expect_true("definition_html" %in% names(grouped))
  expect_equal(grouped$definition_html, "<a>Text.</a>")
})

test_that("IPCC report abbreviations expand to full names", {
  expect_equal(.expand_ipcc_report_name("AR5-WG1"),
               "5th Assessment Report - Working Group I")
  expect_equal(.expand_ipcc_report_name("NOT-A-REPORT"), "NOT-A-REPORT")
  expect_equal(.expand_ipcc_report_name(""), "")
})

test_that("source_inline expands and joins multi-line source lists", {
  expect_equal(.glossary_source_inline("AR5-WG1\nAR5-WG2"),
               paste("5th Assessment Report - Working Group I",
                     "5th Assessment Report - Working Group II", sep = "; "))
  expect_equal(.glossary_source_inline(""), "—")
  expect_equal(.glossary_source_inline(NA), "—")
})
