test_that("clean_html strips tags and decodes entities", {
  expect_equal(clean_html("<p>Water &amp; soil</p>"), "Water & soil")
  expect_equal(clean_html("a&nbsp;&nbsp;b"), "a b")
  expect_equal(clean_html("&lt;tag&gt; &quot;q&quot; &#39;s&#39;"), "<tag> \"q\" 's'")
  expect_equal(clean_html("  spaced   out  "), "spaced out")
})

test_that("clean_html passes through NA and NULL unchanged", {
  expect_true(is.na(clean_html(NA_character_)))
  expect_null(clean_html(NULL))
})

test_that("normalise_term lowercases and strips punctuation", {
  expect_equal(normalise_term("Ecosystem-based Approach"), "ecosystem based approach")
  expect_equal(normalise_term("CO2-equivalent (CO2-eq)"), "co2 equivalent co2 eq")
  expect_equal(normalise_term("  Multiple   spaces "), "multiple spaces")
})

test_that("normalise_term is vectorised and collapses case-only differences", {
  expect_equal(normalise_term(c("Biodiversity", "biodiversity")),
               c("biodiversity", "biodiversity"))
})

test_that("strip_qualifier removes parenthetical qualifiers", {
  expect_equal(strip_qualifier("abundance (ecological)"), "abundance")
  expect_equal(strip_qualifier("carbon dioxide (CO2) capture"), "carbon dioxide capture")
  expect_equal(strip_qualifier("no qualifier"), "no qualifier")
})
