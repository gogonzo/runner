% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{streak_run}
\alias{streak_run}
\title{Running streak length}
\usage{
streak_run(
  x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_rm = TRUE,
  na_pad = FALSE
)
}
\arguments{
\item{x}{{any type} vector which running function is calculated on}

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

\item{idx}{(\code{integer}, \code{Date}, \code{POSIXt})\cr
Optional integer vector containing sorted (ascending) index of observation.
By default \code{idx} is index incremented by one. User can provide index with
varying increment and with duplicated values. If specified then \code{k} and
\code{lag} are depending on \code{idx}. Length of \code{idx} have to be equal of length
\code{x}.}

\item{at}{(\code{integer}, \code{Date}, \code{POSIXt}, \code{character} vector)\cr
Vector of any size and any value defining output data points. Values of the
vector defines the indexes which data is computed at. Can be also \code{POSIXt}
sequence increment used in \code{at} argument in \code{\link[base:seq.POSIXt]{base::seq.POSIXt()}}.
See 'Specifying time-intervals' in details section.}

\item{na_rm}{\code{logical} single value (default \code{na_rm = TRUE}) -
if \code{TRUE} sum is calculating excluding \code{NA}.}

\item{na_pad}{(\code{logical} single value)\cr
Whether incomplete window should return \code{NA} (if \code{na_pad = TRUE})
Incomplete window is when some parts of the window are out of range.}
}
\value{
streak \link{numeric} vector of length equals length of \code{x} containing
number of consecutive occurrences.
}
\description{
Calculates running series of consecutive elements
}
\examples{
set.seed(11)
x1 <- sample(c("a", "b"), 15, replace = TRUE)
x2 <- sample(c(NA_character_, "a", "b"), 15, replace = TRUE)
k <- sample(1:4, 15, replace = TRUE)
streak_run(x1) # simple streak run
streak_run(x1, k = 2) # streak run within 2-element window
streak_run(x2, na_pad = TRUE, k = 3) # streak run within k=3 with padding NA
streak_run(x1, k = k) # streak run within varying window size specified by vector k
}
