test_that("load_ipbes lowercases concepts so case variants group together", {
  dir <- withr::local_tempdir()
  long <- load_ipbes(write_ipbes_csv(dir, default_ipbes_rows()))

  expect_true(all(long$concept == tolower(long$concept)))
  expect_setequal(
    unique(long$concept),
    c("biodiversity", "ecosystem services", "abundance (ecological)", "pollination")
  )
})

test_that("load_ipbes splits comma-separated deliverables into one row each", {
  dir <- withr::local_tempdir()
  long <- load_ipbes(write_ipbes_csv(dir, default_ipbes_rows()))

  es <- long[long$concept == "ecosystem services", ]
  expect_setequal(es$assessment, c("Global assessment", "Values assessment"))
})

test_that("load_ipbes errors clearly when the file is missing", {
  expect_error(load_ipbes(path = file.path(tempdir(), "nope.csv")),
               "IPBES glossary CSV not found")
})

test_that("summarise_ipbes collapses to one row per concept with a detail table", {
  dir <- withr::local_tempdir()
  summary <- summarise_ipbes(load_ipbes(write_ipbes_csv(dir, default_ipbes_rows())))

  expect_equal(nrow(summary), 4L)
  bio <- summary[summary$concept == "biodiversity", ]
  expect_equal(bio$n_assessments, 2L)
  expect_setequal(bio$ipbes_data[[1]]$assessment,
                  c("Global assessment", "Pollination assessment"))
})

test_that("summarise_ipbes prefers a real definition over a 'see X' redirect", {
  rows <- data.frame(
    Concept = c("Ecosystem approach", "Ecosystem approach"),
    Definition = c("See 'Ecosystem-based approach'.",
                   "A strategy for the integrated management of land and water."),
    `Deliverable(s)` = c("Global assessment", "Values assessment"),
    term_id = c(9, 9),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  dir <- withr::local_tempdir()
  summary <- summarise_ipbes(load_ipbes(write_ipbes_csv(dir, rows)))

  expect_false(grepl("^see\\b", summary$summary_definition, ignore.case = TRUE))
})

test_that("load_ipcc returns the documented columns and cleans HTML", {
  dir <- withr::local_tempdir()
  rows <- default_ipcc_rows()
  rows$definition[1] <- "<p>Biodiversity &amp; variability.</p>"
  df <- load_ipcc(cache_dir = dir, bundled_path = write_ipcc_csv(dir, rows))

  expect_equal(names(df), c("id", "term", "report", "definition",
                            "xref_kind", "xref_target", "xref_target_id",
                            "downloaded_at"))
  expect_equal(df$definition[1], "Biodiversity & variability.")
})

test_that("summarise_ipcc keeps each report's own definition", {
  dir <- withr::local_tempdir()
  summary <- summarise_ipcc(load_ipcc(
    cache_dir = dir, bundled_path = write_ipcc_csv(dir, default_ipcc_rows())
  ))

  bio <- summary[summary$term == "Biodiversity", ]
  expect_equal(bio$n_reports, 2L)
  expect_setequal(bio$ipcc_data[[1]]$report, c("AR6", "SR15"))

  # The two reports word the definition differently; both must survive.
  expect_equal(length(unique(bio$ipcc_data[[1]]$definition)), 2L)
  expect_match(bio$ipcc_data[[1]]$definition[bio$ipcc_data[[1]]$report == "SR15"],
               "including within species", fixed = TRUE)
})

test_that("summarise_ipcc keeps pointers out of the definition text", {
  dir <- withr::local_tempdir()
  summary <- summarise_ipcc(load_ipcc(
    cache_dir = dir, bundled_path = write_ipcc_csv(dir, default_ipcc_rows())
  ))

  detail <- summary[summary$term == "Attribution", ]$ipcc_data[[1]]
  srccl <- detail[detail$report == "SRCCL", ]

  # The definition stays empty; the pointer lives in its own field.
  expect_equal(srccl$definition, "")
  expect_equal(srccl$xref, "See: Biodiversity")

  # A report with neither a definition nor a pointer has both blank.
  ar6 <- detail[detail$report == "AR6", ]
  expect_equal(ar6$definition, "")
  expect_equal(ar6$xref, "")
})

test_that("a definition and a pointer can coexist on one report entry", {
  dir <- withr::local_tempdir()
  summary <- summarise_ipcc(load_ipcc(
    cache_dir = dir, bundled_path = write_ipcc_csv(dir, default_ipcc_rows())
  ))

  bio <- summary[summary$term == "Biodiversity", ]$ipcc_data[[1]]
  ar6 <- bio[bio$report == "AR6", ]

  expect_match(ar6$definition, "variability among living organisms", fixed = TRUE)
  expect_equal(ar6$xref, "See also: Pollination")

  # Sub-terms are labelled as such, not as a redirect.
  abundance <- summary[summary$term == "Abundance", ]$ipcc_data[[1]]
  expect_equal(abundance$xref, "Sub-terms: Abundance (ecological)")
})

test_that("load_ipcc still reads the legacy one-row-per-term format", {
  dir <- withr::local_tempdir()
  df <- load_ipcc(cache_dir = dir,
                  bundled_path = write_ipcc_csv(dir, legacy_ipcc_rows()))

  # The semicolon report list is expanded into one row per report.
  expect_equal(nrow(df), 3L)
  expect_setequal(df$report[df$term == "Biodiversity"], c("AR6", "SR15"))
  expect_true(all(is.na(df$xref_target)))
})

test_that("resolve_ipcc_source_path prefers a cached CSV over the bundled one", {
  dir <- withr::local_tempdir()
  bundled <- write_ipcc_csv(withr::local_tempdir(), default_ipcc_rows())

  expect_equal(
    normalizePath(resolve_ipcc_source_path(dir, bundled), mustWork = FALSE),
    normalizePath(bundled, mustWork = FALSE)
  )

  cached <- write_ipcc_csv(dir, default_ipcc_rows())
  expect_equal(
    normalizePath(resolve_ipcc_source_path(dir, bundled), mustWork = FALSE),
    normalizePath(cached, mustWork = FALSE)
  )
})
