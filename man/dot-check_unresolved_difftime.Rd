% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.check_unresolved_difftime}
\alias{.check_unresolved_difftime}
\title{Resolves time difference argument}
\usage{
.check_unresolved_difftime(x, k)
}
\arguments{
\item{x}{(\code{data.frame})}

\item{k}{(\code{integer} vector or single value)\cr
Denoting size of the running window. If \code{k} is a single value then window
size is constant for all elements, otherwise if \code{length(k) == length(x)}
different window size for each element. One can also specify \code{k} in the same
way as \code{by} argument in \code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}.
See 'Specifying time-intervals' in details section.}
}
\value{
resolved \code{idx}
}
\description{
Resolves \code{at} argument passed to the \code{runner} (\code{k} or \code{lag})
checks if the argument has a valid value. If argument is a single character
matching column name in the \code{x} then is replaced with the value \code{x[[idx]]}
}
\keyword{internal}
