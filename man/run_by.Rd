% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_by.R
\name{run_by}
\alias{run_by}
\title{Set window parameters}
\usage{
run_by(x, idx, k, lag, na_pad, at)
}
\arguments{
\item{x}{(\code{vector}, \code{data.frame}, \code{matrix}, \code{xts}, \code{grouped_df})\cr
Input in runner custom function \code{f}.}

\item{idx}{(\code{integer}, \code{Date}, \code{POSIXt})\cr
Optional integer vector containing sorted (ascending) index of observation.
By default \code{idx} is index incremented by one. User can provide index with
varying increment and with duplicated values. If specified then \code{k} and
\code{lag} are depending on \code{idx}. Length of \code{idx} have to be equal of length
\code{x}.}

\item{k}{(\code{integer} vector or single value)\cr
Denoting size of the running window. If \code{k} is a single value then window
size is constant for all elements, otherwise if \code{length(k) == length(x)}
different window size for each element. One can also specify \code{k} in the same
way as \code{by} argument in \code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}.
See 'Specifying time-intervals' in details section.}

\item{lag}{(\code{integer} vector or single value)\cr
Denoting window lag. If \code{lag} is a single value then window lag is constant
for all elements, otherwise if \code{length(lag) == length(x)} different window
size for each element. Negative value shifts window forward. One can also
specify \code{lag} in the same way as \code{by} argument in
\code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}. See 'Specifying time-intervals' in details
section.}

\item{na_pad}{(\code{logical} single value)\cr
Whether incomplete window should return \code{NA} (if \code{na_pad = TRUE})
Incomplete window is when some parts of the window are out of range.}

\item{at}{(\code{integer}, \code{Date}, \code{POSIXt}, \code{character} vector)\cr
Vector of any size and any value defining output data points. Values of the
vector defines the indexes which data is computed at. Can be also \code{POSIXt}
sequence increment used in \code{at} argument in \code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}.
See 'Specifying time-intervals' in details section.}
}
\value{
x object which \code{\link[=runner]{runner()}} can be executed on.
}
\description{
Set window parameters for \code{\link[=runner]{runner()}}. This function sets the
attributes to \code{x} (only \code{data.frame}) object and saves user effort
to specify window parameters in further multiple \code{\link[=runner]{runner()}} calls.
}
\examples{
\dontrun{
library(dplyr)

data <- data.frame(
  index = c(2, 3, 3, 4, 5, 8, 10, 10, 13, 15),
  a = rep(c("a", "b"), each = 5),
  b = 1:10
)

data \%>\%
  group_by(a) \%>\%
  run_by(idx = "index", k = 5) \%>\%
  mutate(
    c = runner(
      x = .,
      f = function(x) {
        paste(x$b, collapse = ">")
      }
    ),
    d = runner(
      x = .,
      f = function(x) {
        sum(x$b)
      }
    )
  )
}
}
