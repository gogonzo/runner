#' Apply running function
#'
#' Applies custom function on running windows.
#' @param x Vector of any type
#' @param k (\code{integer}) vector or single value denoting size of the running
#' window. If \code{k} is a single value then window size is constant for all
#' elements, otherwise if \code{length(k) == length(x)} different window size
#' for each element.
#' @param lag (\code{integer}) vector or single value denoting window lag.
#' If \code{lag} is a single value then window lag is constant for all elements,
#' otherwise if \code{length(lag) == length(x)} different window size for each
#' element. Negative value shifts window forward.
#' @param idx (\code{date or integer}) an optional integer vector containing
#' index of observation. If specified
#' then \code{k} and \code{lag} are depending on \code{idx}. Length of
#' \code{idx} should be equal of length \code{x}
#' @param f \code{function} to be applied on windows created from \code{x}
#' @param at (\code{date or integer}) vector of any size and any value
#' defining output data points. Values of the vector defines the indexes which
#' data is computed at. If \code{idx} is missing then uses indices from \code{1}
#'  to \code{length(x)}, otherwise depends on indexes passed with \code{idx}.
#'  If \code{at} is defined then \code{k} and \code{lag} should be of length equal
#'  one or length of the \code{at}.
#' @param na_pad \code{logical} single value (default \code{na_pad=FALSE}) - if
#'  \code{TRUE} calculation on incomplete window will return \code{NA}.
#'  Incomplete window is when some parts of the window are out of range
#' @param type output type \code{("logical", "numeric", "integer", "character")}.
#'  \code{runner} by default returns numeric values, but if function is expected
#'  to return other type, user should specify this in \code{type} argument.
#' @param ... other arguments passed to the function \code{f}.
#'
#' @return vector with aggregated values for each window. Length of output is the
#' same as \code{length(x)} or \code{length(at)} if specified. Type of the output
#' is taken from \code{type} argument.
#' @examples
#'
#' # mean on k = 3 elements windows
#' runner(1:10, f = mean, k = 3)
#'
#' # mean on k = 3 elements windows with different specification
#' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
#'
#' # number of unique values in each window (varying window size)
#' runner(letters[1:10],
#'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
#'        f = function(x) length(unique(x)))
#'
#' # concatenate window values
#' runner(letters[1:10],
#'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
#'        f = function(x) paste(x, collapse = "-"),
#'        type = "character")
#'
#' # concatenate only on selected windows index
#' runner(letters[1:10],
#'        f = function(x) paste(x, collapse = "-"),
#'        at = c(1, 5, 8),
#'        type = "character")
#'
#' @importFrom methods is
#' @export
runner <- function(x,
                  f,
                  k = integer(0),
                  lag = integer(1),
                  idx = integer(0),
                  at = integer(0),
                  na_pad = FALSE,
                  ...,
                  type = "auto") {

  if (!is(f, "function")) {
    stop("f should be a function")
  }

  w <- window_run(x = x, k = k, lag = lag, idx = idx, at = at, na_pad = na_pad)
  n <- length(w)

  if (type != "auto") {
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
