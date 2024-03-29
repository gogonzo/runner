% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.k_by}
\alias{.k_by}
\title{Converts k and lag from time-unit-interval to int}
\usage{
.k_by(k, idx, param)
}
\arguments{
\item{k}{(\code{integer} vector or single value)\cr
Denoting size of the running window. If \code{k} is a single value then window
size is constant for all elements, otherwise if \code{length(k) == length(x)}
different window size for each element. One can also specify \code{k} in the same
way as \code{by} argument in \code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}.
See 'Specifying time-intervals' in details section.}

\item{idx}{(\code{integer}, \code{Date}, \code{POSIXt})\cr
Optional integer vector containing sorted (ascending) index of observation.
By default \code{idx} is index incremented by one. User can provide index with
varying increment and with duplicated values. If specified then \code{k} and
\code{lag} are depending on \code{idx}. Length of \code{idx} have to be equal of length
\code{x}.}

\item{param}{name of the parameter to be printed in error message}
}
\description{
Converts k and lag from time-unit-interval to int
}
\examples{
k <- "1 month"
idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)
k_difftime <- runner:::.k_by(k, idx, param = "k")
idx - k_difftime
}
\keyword{internal}
