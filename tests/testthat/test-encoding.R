# Guards against the failure mode that silently corrupted both snapshots: R
# running in a non-UTF-8 locale (a bare Rscript usually gets `C`) cannot
# represent the degree signs, en-dashes, curly quotes and transliterated
# Sanskrit in these glossaries, so write.csv() replaces each with a literal
# `<U+XXXX>` escape. clean_html() then strips that escape as if it were an HTML
# tag, so "range of" reached the app as "rangeof".

test_that("mangled escapes are detected", {
  x <- c("clean text", "1.5<U+00B0>C pathway", "ahims<U+0101>", "also clean")

  expect_equal(.find_mangled_encoding(x), c(2L, 3L))
  expect_equal(.find_mangled_encoding("plain"), integer(0))
  expect_equal(.find_mangled_encoding(NA_character_), integer(0))
  expect_equal(.find_mangled_encoding(character(0)), integer(0))
})

test_that("a UTF-8 locale is available and reported", {
  expect_silent(loc <- .ensure_utf8_locale())
  expect_true(isTRUE(l10n_info()$`UTF-8`))
})

test_that("clean_html normalises non-breaking spaces to real spaces", {
  # Left in place, U+00A0 is not matched by PCRE's \\s, which breaks whole-word
  # term matching.
  expect_equal(clean_html("range of observed values"),
               "range of observed values")
  expect_equal(clean_html("a  b"), "a b")
})

test_that("clean_html still strips real tags and decodes entities", {
  expect_equal(clean_html("<p>Water &amp; soil</p>"), "Water & soil")
})

test_that("the bundled snapshots contain no mangled escapes", {
  for (name in c("ipbes_glossary.csv", "ipcc_glossary.csv")) {
    path <- .pkg_file("extdata", name)
    skip_if_not(nzchar(path) && file.exists(path), paste(name, "not available"))

    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                          encoding = "UTF-8")
    offenders <- unlist(lapply(df, function(col) {
      if (!is.character(col)) return(integer(0))
      .find_mangled_encoding(col)
    }))
    expect_equal(length(unique(offenders)), 0L, info = name)
  }
})
