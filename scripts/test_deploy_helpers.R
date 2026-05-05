source(file.path("scripts", "deploy_helpers.R"))

.expect_equal <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(sprintf("FAIL [%s]: expected '%s' got '%s'", label, expected, actual))
  }
  cat(sprintf("OK   [%s]\n", label))
}

.expect_error <- function(expr, pattern, label) {
  ok <- FALSE
  msg <- ""
  tryCatch(
    {
      force(expr)
    },
    error = function(e) {
      msg <<- conditionMessage(e)
      if (grepl(pattern, msg, ignore.case = TRUE)) ok <<- TRUE
    }
  )
  if (!ok) stop(sprintf("FAIL [%s]: expected error pattern '%s', got '%s'", label, pattern, msg))
  cat(sprintf("OK   [%s]\n", label))
}

# Name resolution tests
.expect_equal(resolve_target_name("glossary-ipbes-ipcc", "main"), "glossary-ipbes-ipcc", "main keeps base")
.expect_equal(resolve_target_name("glossary-ipbes-ipcc", "dev"), "glossary-ipbes-ipcc-dev", "dev suffix")
.expect_equal(resolve_target_name("glossary", "feature/ABC+123"), "glossary-feature-abc-123", "sanitize branch")
.expect_equal(resolve_target_name("glossary", "dev", explicit_name = "custom-name"), "custom-name", "explicit override")

# GUID guard tests
known <- c("prod-guid-1", "prod-guid-2")

guard_connect_guid("prod-guid-1", known, branch = "main", allow_prod_overwrite = FALSE)
cat("OK   [prod branch allows known prod guid]\n")

guard_connect_guid("prod-guid-1", known, branch = "dev", allow_prod_overwrite = TRUE)
cat("OK   [override allows non-prod guid target]\n")

.expect_error(
  guard_connect_guid("prod-guid-2", known, branch = "dev", allow_prod_overwrite = FALSE),
  "Refusing deployment",
  "non-prod blocks known prod guid"
)

cat("All deploy helper checks passed.\n")
