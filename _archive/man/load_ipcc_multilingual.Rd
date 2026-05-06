% Generated manually to match exported function in R/data_ipcc.R
\name{load_ipcc_multilingual}
\alias{load_ipcc_multilingual}
\title{Load the multilingual IPCC glossary matrix}
\usage{
load_ipcc_multilingual(
  cache_dir = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
  bundled_path = {
      p <- system.file("extdata", "ipcc_glossary_multilingual.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary_multilingual.csv")
    }
)
}
\arguments{
\item{cache_dir}{Directory where the multilingual IPCC CSV may be stored.
Defaults to \code{tools::R_user_dir()} cache.}

\item{bundled_path}{Path to the bundled multilingual IPCC CSV.}
}
\value{
A \code{tibble::tibble()} with one row per \code{id/report/language}.
}
\description{
Looks for a user-updated multilingual CSV in \code{cache_dir} first; falls back
to the bundled snapshot in \code{inst/extdata/}.
}
