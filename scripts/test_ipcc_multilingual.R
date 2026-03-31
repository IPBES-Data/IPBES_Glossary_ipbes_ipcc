source(file.path("R", "utils.R"))
source(file.path("R", "data_ipcc.R"))

expect <- function(ok, msg) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", msg))
  cat(sprintf("OK: %s\n", msg))
}

# -----------------------------------------------------------------------------
# Parser fixture checks
# -----------------------------------------------------------------------------
occ_html <- "<div><button class='specificlink' data-report='AR5-WG2' data-phraseid='9'></button><button class='specificlink' data-report='AR6' data-phraseid='9'></button></div>"
reports <- .ipcc_parse_report_codes_from_occurrence_html(occ_html)
expect(identical(sort(reports), c("AR5-WG2", "AR6")), "report-code extraction from occurrence html")

detail_html <- paste0(
  "<h5 class='fs-6 bg-primary text-light fw-bold p-2'>Adaptation</h5>",
  "<dd><p>English definition text.</p></dd>",
  "<ul>",
  "<li><a class='translation' data-lang='fr' data-report='AR5-WG2' data-did='5186' data-pid='9'>Français</a></li>",
  "<li><a class='translation' data-lang='es' data-report='AR5-WG2' data-did='5186' data-pid='9'>Español</a></li>",
  "</ul>"
)
detail <- .ipcc_parse_detail_payload(detail_html)
expect(identical(detail$term, "Adaptation"), "detail parser term")
expect(identical(detail$definition, "English definition text."), "detail parser definition")
expect(identical(sort(detail$translations$language), c("es", "fr")), "detail parser translation links")

tr_html <- "<div><h5>Adaptation (fr)</h5><dd><p>Démarche d'ajustement.</p></dd></div>"
tr_payload <- .ipcc_parse_translation_payload(tr_html, "fr")
expect(identical(tr_payload$term, "Adaptation"), "translation parser term cleanup")
expect(identical(tr_payload$definition, "Démarche d'ajustement."), "translation parser definition")

# -----------------------------------------------------------------------------
# ID dedupe fixture check (dual index endpoints)
# -----------------------------------------------------------------------------
orig_fetch <- .ipcc_fetch_html
orig_sleep <- Sys.sleep

Sys.sleep <- function(...) NULL

.ipcc_fetch_html <- function(url, headers, timeout_sec = 20) {
  if (grepl("ajax.searchbyindex.php\\?q=A", url, fixed = FALSE)) {
    return("<span class='alllink' data-phraseid='1'>Term One</span><span class='alllink' data-phraseid='2'>Term Two</span>")
  }
  if (grepl("ajax.searchbylatestindex.php\\?q=A", url, fixed = FALSE)) {
    return("<span class='alllink' data-phraseid='2'>Term Two</span><span class='alllink' data-phraseid='3'>Term Three</span>")
  }
  ""
}

prefixes <- c("A")
a <- .ipcc_collect_term_stubs("https://apps.ipcc.ch/glossary", NULL, prefixes, "ajax.searchbyindex.php")
b <- .ipcc_collect_term_stubs("https://apps.ipcc.ch/glossary", NULL, prefixes, "ajax.searchbylatestindex.php")
d <- rbind(a, b)
d <- d[order(d$id, d$endpoint), , drop = FALSE]
d <- d[!duplicated(d$id), , drop = FALSE]
expect(nrow(d) == 3, "dual endpoint id dedupe keeps unique ids")

# -----------------------------------------------------------------------------
# Offline integration smoke for matrix output
# -----------------------------------------------------------------------------
.ipcc_fetch_html <- function(url, headers, timeout_sec = 20) {
  # Seed endpoints (only A returns data; other prefixes empty)
  if (grepl("ajax.searchbyindex.php", url, fixed = TRUE)) {
    if (grepl("q=A", url, fixed = TRUE)) {
      return("<span class='alllink' data-phraseid='9'>Adaptation</span>")
    }
    return("")
  }
  if (grepl("ajax.searchbylatestindex.php", url, fixed = TRUE)) {
    if (grepl("q=A", url, fixed = TRUE)) {
      return("<span class='alllink' data-phraseid='9'>Adaptation</span>")
    }
    return("")
  }

  # Occurrence gives two reports
  if (grepl("ajax.searchalloccurance.php\\?q=9&r=", url, fixed = FALSE)) {
    return("<button class='specificlink' data-report='AR5-WG2' data-phraseid='9'></button><button class='specificlink' data-report='AR6' data-phraseid='9'></button>")
  }

  # English detail with translations only for AR5-WG2
  if (grepl("ajax.searchbyphraseandreport.php\\?q=9&r=AR5-WG2", url, fixed = FALSE)) {
    return(paste0(
      "<h5>Adaptation</h5>",
      "<dd><p>English AR5 definition.</p></dd>",
      "<a class='translation' data-lang='fr' data-report='AR5-WG2' data-did='5186' data-pid='9'>Français</a>",
      "<a class='translation' data-lang='es' data-report='AR5-WG2' data-did='5186' data-pid='9'>Español</a>"
    ))
  }
  if (grepl("ajax.searchbyphraseandreport.php\\?q=9&r=AR6", url, fixed = FALSE)) {
    return("<h5>Adaptation</h5><dd><p>English AR6 definition.</p></dd>")
  }

  # Translation payloads
  if (grepl("ajax.gettranslation.php", url, fixed = TRUE) && grepl("l=fr", url, fixed = TRUE)) {
    return("<h5>Adaptation (fr)</h5><dd><p>Définition FR.</p></dd>")
  }
  if (grepl("ajax.gettranslation.php", url, fixed = TRUE) && grepl("l=es", url, fixed = TRUE)) {
    return("<h5>Adaptation (es)</h5><dd><p>Definición ES.</p></dd>")
  }

  ""
}

tmp <- tempfile("ipcc_multi_test_")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
out_path <- scrape_ipcc_multilingual(cache_dir = tmp)
expect(file.exists(out_path), "multilingual output file created")

df <- utils::read.csv(out_path, stringsAsFactors = FALSE)
expect(all(c("id", "report", "language", "fetch_status") %in% names(df)), "multilingual schema columns exist")
expect(nrow(df) == 12, "matrix has 2 reports x 6 languages")

key <- paste(df$id, df$report, df$language, sep = "::")
expect(length(unique(key)) == nrow(df), "matrix keys id/report/language are unique")

fr_ar5 <- df[df$report == "AR5-WG2" & df$language == "fr", , drop = FALSE]
expect(nrow(fr_ar5) == 1 && identical(fr_ar5$fetch_status[[1]], "ok"), "available translation fetched")

ru_ar6 <- df[df$report == "AR6" & df$language == "ru", , drop = FALSE]
expect(nrow(ru_ar6) == 1 && identical(ru_ar6$fetch_status[[1]], "not_available"), "missing translation placeholder persisted")

# Restore
.ipcc_fetch_html <- orig_fetch
Sys.sleep <- orig_sleep

cat("All multilingual IPCC tests passed.\n")
