% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gvs_core.R
\name{gvs_core}
\alias{gvs_core}
\title{Handle API access and format}
\usage{
gvs_core(
  url = "https://gvsapi.xyz/gvs_api.php",
  mode,
  data_json = NULL,
  batches = NULL,
  skip_internet_check = FALSE
)
}
\arguments{
\item{url}{Server URL to use.  Defaults to the stable production version}

\item{mode}{API mode to use.}

\item{data_json}{Either NULL (the default) or properly formatted json}

\item{batches}{NULL or Numeric.  Optional number of batches to divide the request into for parallel processing.}
}
\description{
Internal function for handling accessing the API,data formatting, and errors
}
\keyword{internal}
