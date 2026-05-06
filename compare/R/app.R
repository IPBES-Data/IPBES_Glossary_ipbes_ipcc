# Package entry point
# =============================================================================

#' Run the IPBES/IPCC Glossary Comparison Shiny App
#'
#' Launches the interactive Shiny application that displays IPBES and IPCC
#' glossary definitions side-by-side with similarity scores and word-level
#' diffs.
#'
#' @param cache_dir Directory used to store:
#'   * The user-updated IPCC glossary CSV (written by the "Update" button).
#'
#'   Defaults to a package-specific subdirectory of the OS user cache directory
#'   as returned by [tools::R_user_dir()].  The directory is created
#'   automatically if it does not exist.
#' @param enable_live_update Logical; whether the "Update IPCC Glossary" control
#'   is enabled. Defaults to `.default_live_update_enabled()`, which disables
#'   live updates on hosted shinyapps.io deployments and enables them locally.
#' @param ... Additional arguments passed to [shiny::shinyApp()] (e.g.
#'   `launch.browser`, `port`).
#' @return Invisibly, the [shiny::shinyApp()] object (only relevant when
#'   `launch.browser = FALSE`).
#' @examples
#' \dontrun{
#' glossary.ipbes.ipcc::run_app()
#' }
#' @export
run_app <- function(
    cache_dir = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    enable_live_update = .default_live_update_enabled(),
    ...
) {
  app <- .create_shiny_app(
    cache_dir = cache_dir,
    enable_live_update = enable_live_update
  )

  shiny::runApp(app, ...)
}

# Resolve package version safely.
# - Prefer DESCRIPTION version (source checkout / appDir deployments)
# - Fall back to installed package version
.package_version_safe <- function(package = "glossary.ipbes.ipcc") {
  root <- tryCatch(
    if (exists(".source_pkg_root", mode = "function")) .source_pkg_root() else "",
    error = function(e) ""
  )

  desc_candidates <- c(
    if (nzchar(root)) file.path(root, "DESCRIPTION") else "",
    file.path(getwd(), "DESCRIPTION")
  )
  desc_candidates <- unique(desc_candidates[nzchar(desc_candidates)])

  for (desc_path in desc_candidates) {
    if (!file.exists(desc_path)) next
    dcf <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (!is.null(dcf) && "Version" %in% colnames(dcf)) {
      ver <- trimws(dcf[1, "Version"])
      if (nzchar(ver)) return(ver)
    }
  }

  ver <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) ""
  )
  if (!nzchar(ver)) "unknown" else ver
}

# Build the runnable shinyApp object with preloaded data.
.create_shiny_app <- function(cache_dir, enable_live_update) {
  .ensure_cache_dir(cache_dir)
  merged <- .load_merged_data(cache_dir, prepare_table_cache = TRUE)
  .register_www_assets()

  # ---- Launch app ----------------------------------------------------------
  app <- shiny::shinyApp(
    ui     = build_ui(enable_live_update = enable_live_update),
    server = build_server(
      cache_dir = cache_dir,
      initial_data = merged,
      enable_live_update = enable_live_update
    )
  )

  app
}

# Ensure cache directory exists.
.ensure_cache_dir <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  invisible(cache_dir)
}

# Load merged glossary data with cache-first startup behavior.
.load_merged_data <- function(cache_dir, prepare_table_cache = TRUE) {
  .ensure_cache_dir(cache_dir)

  bundled_ipcc <- .pkg_file("extdata", "ipcc_glossary.csv")
  bundled_ipbes <- .pkg_file("extdata", "ipbes_glossary.csv")
  ipcc_source  <- resolve_ipcc_source_path(cache_dir, bundled_ipcc)
  using_bundled_ipcc <- nzchar(ipcc_source) &&
    nzchar(bundled_ipcc) &&
    identical(
      normalizePath(ipcc_source, winslash = "/", mustWork = FALSE),
      normalizePath(bundled_ipcc, winslash = "/", mustWork = FALSE)
    )

  loaded_from <- "rebuild"
  merged <- if (using_bundled_ipcc) .load_packaged_merged_cache() else NULL
  if (!is.null(merged)) {
    loaded_from <- "packaged"
  }

  cache_meta <- .build_startup_cache_meta(cache_dir)
  if (is.null(merged)) {
    merged <- .load_startup_merged_cache(cache_dir, cache_meta)
    if (!is.null(merged)) loaded_from <- "startup"
  }

  if (is.null(merged)) {
    message("Loading IPBES glossary\u2026")
    ipbes_long <- load_ipbes(path = bundled_ipbes)
    ipbes_sum  <- summarise_ipbes(ipbes_long)

    message("Loading IPCC glossary\u2026")
    ipcc_raw   <- load_ipcc(cache_dir, bundled_path = bundled_ipcc)
    ipcc_sum   <- summarise_ipcc(ipcc_raw)

    message("Merging glossaries\u2026")
    merged <- merge_glossaries(ipbes_sum, ipcc_sum)
    .save_startup_merged_cache(cache_dir, cache_meta, merged)
  } else if (identical(loaded_from, "packaged")) {
    message("Loaded merged glossary from packaged cache.")
  } else if (identical(loaded_from, "startup")) {
    message("Loaded merged glossary from startup cache.")
  }

  if (isTRUE(prepare_table_cache) && !.has_current_table_view_cache(merged)) {
    message("Preparing table view cache...")
    merged <- .prepare_table_data(merged)
    .save_startup_merged_cache(cache_dir, cache_meta, merged)
  }

  n_ipbes   <- sum(!is.na(merged$ipbes_concept))
  n_ipcc    <- sum(!is.na(merged$ipcc_term))
  n_matched <- sum(!is.na(merged$ipbes_concept) & !is.na(merged$ipcc_term))
  message(sprintf(
    "Ready: %d IPBES terms, %d IPCC terms, %d matched.",
    n_ipbes, n_ipcc, n_matched
  ))

  merged
}

# Serve static assets from inst/www once per R session.
.register_www_assets <- function() {
  www_path <- .pkg_file("www")
  if (!nzchar(www_path)) stop("Could not locate inst/www assets.")

  existing <- shiny::resourcePaths()
  if ("custom" %in% names(existing)) {
    existing_path <- normalizePath(existing[["custom"]], winslash = "/", mustWork = FALSE)
    target_path <- normalizePath(www_path, winslash = "/", mustWork = FALSE)
    if (!identical(existing_path, target_path)) {
      shiny::removeResourcePath("custom")
      shiny::addResourcePath("custom", www_path)
    }
  } else {
    shiny::addResourcePath("custom", www_path)
  }
  invisible(www_path)
}

# Build metadata for startup cache invalidation based on source files.
.build_startup_cache_meta <- function(cache_dir) {
  ipbes_path <- .pkg_file("extdata", "ipbes_glossary.csv")
  ipcc_path  <- resolve_ipcc_source_path(
    cache_dir,
    bundled_path = .pkg_file("extdata", "ipcc_glossary.csv")
  )

  list(
    schema      = 1L,
    app_version = .package_version_safe(),
    ipbes       = .file_signature(ipbes_path),
    ipcc        = .file_signature(ipcc_path)
  )
}

# Metadata for bundled merged cache validation.
.build_packaged_cache_meta <- function() {
  ipbes_path <- .pkg_file("extdata", "ipbes_glossary.csv")
  ipcc_path  <- .pkg_file("extdata", "ipcc_glossary.csv")

  list(
    schema   = 1L,
    ipbes_md5 = .file_md5(ipbes_path),
    ipcc_md5  = .file_md5(ipcc_path)
  )
}

# Compute md5 for a file (or NA if missing).
.file_md5 <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NA_character_)
  unname(as.character(tools::md5sum(path)[[1]]))
}

# Return a stable signature of a source CSV file.
.file_signature <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(list(path = path, size = NA_real_, mtime = NA_character_))
  }
  info <- file.info(path)
  list(
    path  = normalizePath(path, winslash = "/", mustWork = FALSE),
    size  = as.numeric(info$size),
    mtime = as.character(info$mtime)
  )
}

# Load cached merged data when source signatures match.
.load_startup_merged_cache <- function(cache_dir, expected_meta) {
  path <- file.path(cache_dir, "startup_merged_cache.rds")
  if (!file.exists(path)) return(NULL)

  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || is.null(obj$meta) || is.null(obj$merged)) {
    return(NULL)
  }
  if (!identical(obj$meta, expected_meta)) return(NULL)

  required_cols <- c(
    "matched_term", "ipbes_concept", "ipcc_term",
    "sim_within_ipbes", "sim_within_ipcc", "sim_between_all",
    "ipbes_data", "ipcc_data"
  )
  if (!all(required_cols %in% names(obj$merged))) return(NULL)

  obj$merged
}

# Load packaged merged cache shipped in inst/extdata.
.load_packaged_merged_cache <- function() {
  path <- .pkg_file("extdata", "merged_glossary_cache.rds")
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || is.null(obj$meta) || is.null(obj$merged)) {
    return(NULL)
  }
  if (!identical(obj$meta, .build_packaged_cache_meta())) return(NULL)

  required_cols <- c(
    "matched_term", "ipbes_concept", "ipcc_term",
    "sim_within_ipbes", "sim_within_ipcc", "sim_between_all",
    "ipbes_data", "ipcc_data"
  )
  if (!all(required_cols %in% names(obj$merged))) return(NULL)

  obj$merged
}

# Discover source package root (when running from a checkout).
.source_pkg_root <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)

    dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    repeat {
      desc <- file.path(dir, "DESCRIPTION")
      if (file.exists(desc)) {
        pkg_line <- grep(
          "^Package\\s*:",
          readLines(desc, warn = FALSE),
          value = TRUE
        )
        pkg_name <- if (length(pkg_line) > 0) {
          trimws(sub("^Package\\s*:\\s*", "", pkg_line[[1]]))
        } else {
          ""
        }

        if (identical(pkg_name, "glossary.ipbes.ipcc")) {
          cached <<- dir
          return(dir)
        }
      }

      parent <- dirname(dir)
      if (identical(parent, dir)) break
      dir <- parent
    }

    cached <<- ""
    ""
  }
})

# Locate a packaged file. Falls back to local `inst/` when running from source.
.pkg_file <- function(...) {
  source_root <- .source_pkg_root()
  if (nzchar(source_root)) {
    source_path <- file.path(source_root, "inst", ...)
    if (file.exists(source_path) || dir.exists(source_path)) return(source_path)
  }

  pkg_path <- system.file(..., package = "glossary.ipbes.ipcc")
  if (nzchar(pkg_path)) return(pkg_path)

  local_path <- file.path(getwd(), "inst", ...)
  if (file.exists(local_path) || dir.exists(local_path)) return(local_path)
  ""
}

# Save merged data for future fast startups.
.save_startup_merged_cache <- function(cache_dir, meta, merged) {
  path <- file.path(cache_dir, "startup_merged_cache.rds")
  tryCatch(
    saveRDS(list(meta = meta, merged = merged), path),
    error = function(e) NULL
  )
  invisible(merged)
}

# Default runtime switch for live updates:
# - Local/dev: enabled
# - shinyapps.io hosted runtime: disabled
# - Explicit env var GLOSSARY_ENABLE_LIVE_UPDATE overrides both
.default_live_update_enabled <- function() {
  raw <- trimws(Sys.getenv("GLOSSARY_ENABLE_LIVE_UPDATE", ""))
  if (nzchar(raw)) {
    val <- tolower(raw)
    if (val %in% c("1", "true", "t", "yes", "y", "on")) return(TRUE)
    if (val %in% c("0", "false", "f", "no", "n", "off")) return(FALSE)
  }

  !.is_shinyapps_runtime()
}

# Detect shinyapps.io runtime using standard env markers.
.is_shinyapps_runtime <- function() {
  identical(tolower(Sys.getenv("R_CONFIG_ACTIVE", "")), "shinyapps") ||
    (nzchar(Sys.getenv("SHINY_PORT", "")) && nzchar(Sys.getenv("SHINY_HOST", "")))
}
