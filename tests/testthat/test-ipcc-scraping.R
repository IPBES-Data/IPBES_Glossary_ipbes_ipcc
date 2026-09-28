# Parsers for the two IPCC AJAX responses. These are pure functions over HTML,
# so they are tested against fixture markup that mirrors the live structure.

occurrences_html <- function() {
  paste0(
    "<div><h4>Likelihood</h4>",
    "<dd><h5>Special Report on Climate Change and Land - SRCCL ",
    "<button data-report='SRCCL' data-phraseid='7'></button></h5>",
    "<p>The chance of a specific outcome occurring.</p></dd>",
    "<dd><h5>AR6 <button data-report='AR6' data-phraseid='7'></button></h5>",
    "<p>A probabilistic estimate of the occurrence of a single event.</p></dd>",
    "<dd><h5>AR5-WG1 <button data-report='AR5-WG1' data-phraseid='7'></button></h5>",
    "<p></p></dd></div>"
  )
}

see_html <- function() {
  paste0(
    "<h5>Attribution</h5><dd><p>&nbsp;</p>",
    "<div class='small'><h6>See/ See Also...</h6><ul class='items'>",
    "<li> See... <span class='specificlink' data-report='SRCCL' ",
    "data-phrase='Detection and attribution' data-phraseid='187'>",
    "Detection and attribution</span></li></ul></div></dd>"
  )
}

see_also_html <- function() {
  paste0(
    "<h5>Fuel poverty</h5><dd><p>A real definition.</p>",
    "<div class='small'><h6>See/ See Also...</h6><ul class='items'>",
    "<li> See Also... <span class='specificlink' data-phrase='Energy poverty' ",
    "data-phraseid='2944'>Energy poverty</span></li></ul></div></dd>"
  )
}

test_that("occurrences parse into one row per report", {
  occ <- .ipcc_parse_occurrences(occurrences_html())

  expect_equal(nrow(occ), 3L)
  expect_equal(occ$report, c("SRCCL", "AR6", "AR5-WG1"))
})

test_that("each report keeps its own wording", {
  occ <- .ipcc_parse_occurrences(occurrences_html())

  # The whole point of the per-report schema: these must not be collapsed to
  # the first report's text.
  expect_match(occ$definition[1], "chance of a specific outcome", fixed = TRUE)
  expect_match(occ$definition[2], "probabilistic estimate", fixed = TRUE)
  expect_equal(length(unique(occ$definition[nzchar(occ$definition)])), 2L)
})

test_that("a report with no definition yields an empty string, not a drop", {
  occ <- .ipcc_parse_occurrences(occurrences_html())

  expect_equal(occ$definition[occ$report == "AR5-WG1"], "")
})

test_that("occurrences parsing degrades gracefully", {
  for (input in list(NULL, "", "   ", "<html><body>nothing</body></html>")) {
    occ <- .ipcc_parse_occurrences(input)
    expect_equal(nrow(occ), 0L)
    expect_equal(names(occ), c("report", "definition"))
  }
})

test_that("a 'See' redirect is parsed with its target and id", {
  xref <- .ipcc_parse_cross_reference(see_html())

  expect_equal(xref$xref_kind, "see")
  expect_equal(xref$xref_target, "Detection and attribution")
  expect_equal(xref$xref_target_id, "187")
})

test_that("'See Also' is distinguished from 'See'", {
  expect_equal(.ipcc_parse_cross_reference(see_also_html())$xref_kind, "see_also")
})

sub_terms_html <- function() {
  paste0(
    "<h5>Climate sensitivity</h5><dd><p>&nbsp;</p>",
    "<div class='small'><h6>Sub-terms</h6><ul class='items'>",
    "<li><span data-phrase='Equilibrium climate sensitivity' ",
    "data-phraseid='1'>Equilibrium climate sensitivity</span></li>",
    "<li><span data-phrase='Transient climate response' ",
    "data-phraseid='2'>Transient climate response</span></li>",
    "</ul></div></dd>"
  )
}

test_that("a 'Sub-terms' list is not mistaken for a See redirect", {
  xref <- .ipcc_parse_cross_reference(sub_terms_html())

  # These items carry no "See" prefix; only the <h6> heading identifies them.
  expect_equal(xref$xref_kind, "sub_terms")
  expect_equal(xref$xref_target,
               "Equilibrium climate sensitivity | Transient climate response")
})

test_that("multiple targets are stored pipe-joined", {
  xref <- .ipcc_parse_cross_reference(sub_terms_html())
  expect_equal(length(strsplit(xref$xref_target, " | ", fixed = TRUE)[[1]]), 2L)
})

test_that("the display label follows the relationship", {
  mk <- function(kind, target) {
    .ipcc_xref_display(data.frame(
      xref_kind = kind, xref_target = target, stringsAsFactors = FALSE
    ))
  }

  expect_equal(mk("see", "Detection and attribution"),
               "See: Detection and attribution")
  expect_equal(mk("see_also", "Energy poverty"), "See also: Energy poverty")
  expect_equal(mk("sub_terms", "Snow cover extent"), "Sub-terms: Snow cover extent")
  # A redirect wins when a row carries both kinds.
  expect_equal(mk("see | see_also", "A"), "See: A")
  # Unknown kind degrades to the redirect wording rather than erroring.
  expect_equal(mk(NA_character_, "A"), "See: A")
})

test_that("the pipe storage delimiter never reaches displayed text", {
  shown <- .ipcc_xref_display(data.frame(
    xref_kind = "see_also",
    xref_target = "Decadal variability | Internal variability | Climate change",
    stringsAsFactors = FALSE
  ))

  expect_false(grepl("|", shown, fixed = TRUE))
  expect_equal(shown,
    "See also: Decadal variability, Internal variability, Climate change")
})

test_that("rows without pointers render an empty pointer string", {
  expect_equal(
    .ipcc_xref_display(data.frame(xref_kind = NA_character_,
                                  xref_target = NA_character_,
                                  stringsAsFactors = FALSE)),
    ""
  )
})

test_that("absent cross-references return NA rather than erroring", {
  for (input in list(NULL, "", "<div><dd><p>Just a definition.</p></dd></div>")) {
    xref <- .ipcc_parse_cross_reference(input)
    expect_true(is.na(xref$xref_kind))
    expect_true(is.na(xref$xref_target))
  }
})

test_that("the term index parser tolerates a missing response", {
  # .ipcc_get returns NULL on failure; the index builder must not error.
  expect_silent(occ <- .ipcc_parse_occurrences(NULL))
})
