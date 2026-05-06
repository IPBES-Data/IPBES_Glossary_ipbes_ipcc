# Activate renv only when its library is present (local dev).
# On Posit Connect the platform installs packages via manifest.json into its
# own library, so renv must not redirect the library path.
local({
  lib <- file.path("renv", "library")
  if (dir.exists(lib) && length(list.files(lib, recursive = TRUE)) > 0) {
    source("renv/activate.R")
  }
})
