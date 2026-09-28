test_that("cache metadata changes when a source file changes", {
  dir <- withr::local_tempdir()
  path <- write_ipcc_csv(dir, default_ipcc_rows())

  before <- .file_signature(path)
  rows <- default_ipcc_rows()
  rows$definition[1] <- "A different definition entirely."
  Sys.sleep(1.1)  # ensure a distinct mtime
  write_ipcc_csv(dir, rows)
  after <- .file_signature(path)

  expect_false(identical(before, after))
})

test_that("file signature and md5 report missing files without erroring", {
  missing <- file.path(tempdir(), "definitely-absent.csv")

  expect_true(is.na(.file_signature(missing)$size))
  expect_true(is.na(.file_md5(missing)))
})

test_that("the startup cache round-trips when metadata matches", {
  dir <- withr::local_tempdir()
  merged <- make_merged()
  meta <- list(schema = 1L, app_version = "test")

  .save_startup_merged_cache(dir, meta, merged)
  expect_equal(.load_startup_merged_cache(dir, meta)$matched_term, merged$matched_term)
})

test_that("the startup cache is rejected when metadata differs", {
  dir <- withr::local_tempdir()
  .save_startup_merged_cache(dir, list(schema = 1L, app_version = "a"), make_merged())

  expect_null(.load_startup_merged_cache(dir, list(schema = 1L, app_version = "b")))
})

test_that("a missing or malformed cache file is ignored rather than fatal", {
  dir <- withr::local_tempdir()
  expect_null(.load_startup_merged_cache(dir, list(schema = 1L)))

  saveRDS(list(nonsense = TRUE), file.path(dir, "startup_merged_cache.rds"))
  expect_null(.load_startup_merged_cache(dir, list(schema = 1L)))
})

test_that("the bundled packaged cache is present, valid and pre-rendered", {
  skip_if_not(nzchar(.pkg_file("extdata", "merged_glossary_cache.rds")),
              "packaged cache not available")

  merged <- .load_packaged_merged_cache()
  expect_false(is.null(merged))
  expect_gt(nrow(merged), 1000L)
  expect_true(.has_current_highlight_cache(merged))
})
