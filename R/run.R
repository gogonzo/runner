#' Apply running function
#'
#' Applies custom function on running windows.
#' @param x  to be input in runner custom
#'  function \code{f}.
#'
#' @param k  vector or single value denoting size of the running
#'  window. If \code{k} is a single value then window size is constant for all
#'  elements, otherwise if \code{length(k) == length(x)} different window size
#'  for each element.
#'
#' @param lag  vector or single value denoting window lag.
#'  If \code{lag} is a single value then window lag is constant for all elements,
#'  otherwise if \code{length(lag) == length(x)} different window size for each
#'  element. Negative value shifts window forward.
#'
#' @param idx  an optional integer vector containing
#'  sorted (ascending) index of observation. If specified then \code{k} and
#'  \code{lag} are depending on \code{idx}.
#'  Length of \code{idx} should be equal of length \code{x}.
#'
#' @param f to be applied on windows created from \code{x}
#'
#' @param at vector of any size and any value
#'  defining output data points. Values of the vector defines the indexes which
#'  data is computed at. Can be also \code{POSIXt} sequence increment
#'  \code{\link[base]{seq.POSIXt}}. More in details.
#'
#' @param na_pad single value (default \code{na_pad = FALSE}) - if
#'  \code{TRUE} calculation on incomplete window will return \code{NA}.
#'  Incomplete window is when some parts of the window are out of range
#'
#' @param type output type (\code{"logical"}, \code{"numeric"}, \code{"integer"}, \code{"character"}, \code{"auto"}).
#'  \code{runner} by default returns type automatically. In case of failure of \code{"auto"}
#'  please specify desired type.
#'
#' @param ... other arguments passed to the function \code{f}.
#'
#' @details
#' Function can apply any R function on running windows defined by \code{x},
#' \code{k}, \code{lag}, \code{idx} and \code{at}. Running window can be calculated
#' on several ways:
#' \itemize{
#'  \item{**Cumulative windows**}{\cr
#'    applied when user doesn't specify \code{k} argument or specify \code{k = length(x)},
#'    this would mean that \code{k} is equal to number of available elements \cr
#'    \if{html}{\figure{cumulative_windows.png}{options: width="75\%" alt="Figure: cumulative_windows.png"}}
#'    \if{latex}{\figure{cumulative_windows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Incremental index**}{\cr
#'    applied when user specify \code{k} as constant value keeping \code{idx} and
#'    \code{at} unspecified. \code{lag} argument shifts windows left (\code{lag > 0})
#'    or right (\code{lag < 0}). \cr
#'    \if{html}{\figure{incremental_index.png}{options: width="75\%" alt="Figure: incremental_index.png"}}
#'    \if{latex}{\figure{incremental_index.pdf}{options: width=7cm}}
#'  }
#'  \item{**Windows depending on date**}{\cr
#'    If one specifies \code{idx} this would mean that output windows size might
#'    change in size because of inequally spaced indexes. Fox example 5-period
#'    window is different than 5-element window, because 5-period window might
#'    contain any number of observation (7-day mean is not the same as 7-element mean)
#'     \cr
#'    \if{html}{\figure{running_date_windows.png}{options: width="75\%" alt="Figure: running_date_windows.png"}}
#'    \if{latex}{\figure{running_date_windows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Window at specific indices**}{\cr
#'    \code{runner} by default returns vector of the same size as \code{x} unless one specifies
#'    \code{at} argument. Each element of \code{at} is an index on which runner calculates function -
#'    which means that output of the runner is now of length equal to \code{at}. Note
#'    that one can change index of \code{x} by specifying \code{idx}.
#'    Illustration below shows output of \code{runner} for \code{at = c(13, 27, 45, 31)}
#'    which gives windows in ranges enclosed in square brackets. Range for \code{at = 27} is
#'    \code{[22, 26]} which is not available in current indices. \cr
#'    \if{html}{\figure{runner_at.png}{options: width="75\%" alt="Figure: runner_at.png"}}
#'    \if{latex}{\figure{runner_at.pdf}{options: width=7cm}}\cr
#'
#'    Above is not enough since \code{k} and \code{lag} can be a vector which allows to
#'    stretch and lag/lead each window freely on in time (on indices).
#'  }
#' }
#'
#' @return vector with aggregated values for each window. Length of output is the
#'  same as \code{length(x)} or \code{length(at)} if specified. Type of the output
#'  is taken from \code{type} argument.
#'
#' @examples
#'
#' # mean on k = 3 elements windows
#' runner(1:10, f = mean, k = 3)
#'
#' # mean on k = 3 elements windows with different specification
#' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
#'
#' # concatenate two columns
#' runner(
#'   data.frame(
#'     a = letters[1:10],
#'     b = 1:10
#'   ),
#'   f = function(x) paste(paste0(x$a, x$b), collapse = "+"),
#'   type = "character"
#' )
#'
#' # concatenate two columns with additional argument
#' runner(
#'   data.frame(
#'     a = letters[1:10],
#'     b = 1:10
#'   ),
#'   f = function(x, xxx) {
#'     paste(paste0(x$a, xxx, x$b), collapse = " + ")
#'   },
#'   xxx = "...",
#'   type = "character"
#' )
#'
#' # number of unique values in each window (varying window size)
#' runner(letters[1:10],
#'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
#'        f = function(x) length(unique(x)))
#'
#' # concatenate only on selected windows index
#' runner(letters[1:10],
#'        f = function(x) paste(x, collapse = "-"),
#'        at = c(1, 5, 8),
#'        type = "character")
#'
#' @md
#' @importFrom methods is
#' @export
runner <- function(
  x,
  f,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type = "auto",
  ...
  ) {

  if (!is(f, "function")) {
    stop("f should be a function")
  }

  w <- window_run(
    x = `if`(is.data.frame(x), seq_len(nrow(x)), x),
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )

  if (is.data.frame(x) || is.matrix(x)) {
    res <- sapply(w, function(ww) {
      if (length(ww) == 0) {
        NA
      } else {
        f(x[ww, ], ...)
      }
    })

  } else if (type != "auto") {
    n <- length(w)
    res <- vector(mode = type, length = n)
    for (i in seq_len(n)) {
      ww <- w[[i]]
      res[i] <- if (length(ww) == 0) {
        NA
      } else {
        f(ww, ...)
      }
    }
  } else {
    res <- sapply(w, function(ww)
      if (is.null(ww)) {
        NA
      } else {
        f(ww, ...)
      }
    )
  }


  return(res)
}
