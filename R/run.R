#' Apply running function
#'
#' Applies custom function on running windows.
#' @param x (`vector`, `data.frame`, `matrix`, `xts`, `grouped_df`)\cr
#'  Input in runner custom function `f`.
#'
#' @param k (`integer` vector or single value)\cr
#'  Denoting size of the running window. If `k` is a single value then window
#'  size is constant for all elements, otherwise if `length(k) == length(x)`
#'  different window size for each element. One can also specify `k` in the same
#'  way as `by` argument in \code{\link[base]{seq.POSIXt}}.
#'  See 'Specifying time-intervals' in details section.
#'
#' @param lag (`integer` vector or single value)\cr
#'  Denoting window lag. If `lag` is a single value then window lag is constant
#'  for all elements, otherwise if `length(lag) == length(x)` different window
#'  size for each element. Negative value shifts window forward. One can also
#'  specify `lag` in the same way as `by` argument in
#'  \code{\link[base]{seq.POSIXt}}. See 'Specifying time-intervals' in details
#'  section.
#'
#' @param idx (`integer`, `Date`, `POSIXt`)\cr
#'  Optional integer vector containing sorted (ascending) index of observation.
#'  By default `idx` is index incremented by one. User can provide index with
#'  varying increment and with duplicated values. If specified then `k` and
#'  `lag` are depending on `idx`. Length of `idx` have to be equal of length
#'  `x`.
#'
#' @param f (`function`)\cr
#'  Applied on windows created from `x`. This function is meant to summarize
#'  windows and create single element for each window, but one can also specify
#'  function which return multiple elements (runner output will be a list).
#'  By default runner returns windows as is (`f = function(x)`).
#'
#' @param at (`integer`, `Date`, `POSIXt`, `character` vector)\cr
#'  Vector of any size and any value defining output data points. Values of the
#'  vector defines the indexes which data is computed at. Can be also `POSIXt`
#'  sequence increment used in `at` argument in \code{\link[base]{seq.POSIXt}}.
#'  See 'Specifying time-intervals' in details section.
#'
#' @param na_pad (`logical` single value)\cr
#'  Whether incomplete window should return `NA` (if `na_pad = TRUE`)
#'  Incomplete window is when some parts of the window are out of range.
#'
#' @param simplify (`logical` or `character` value)\cr
#'  should the result be simplified to a vector, matrix or higher dimensional
#'  array if possible. The default value, `simplify = TRUE`, returns a vector or
#'  matrix if appropriate, whereas if `simplify = "array"` the result may be an
#'  array of "rank" `(=length(dim(.)))` one higher than the result of output
#'  from the function `f` for each window. Consequences of `simplify` in `runner`
#'  are identical to `sapply`.
#'
#' @param cl (`cluster`) *experimental*\cr
#'  Create and pass the cluster to the `runner` function to run each window
#'  calculation in parallel. See \code{\link[parallel]{makeCluster}} in details.
#'
#' @param ... (optional)\cr
#'   other arguments passed to the function `f`.
#'
#' @details
#' Function can apply any R function on running windows defined by `x`,
#' `k`, `lag`, `idx` and `at`. Running window can be calculated
#' on several ways:
#' \itemize{
#'  \item{**Cumulative windows**}{\cr
#'    applied when user doesn't specify `k` argument or specify `k = length(x)`,
#'    this would mean that `k` is equal to number of available elements \cr
#'    \if{html}{\figure{cumulativewindows.png}{options: width="75\%" alt="Figure: cumulativewindows.png"}}
#'    \if{latex}{\figure{cumulativewindows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Constant sliding windows**}{\cr
#'    applied when user specify `k` as constant value keeping `idx` and
#'    `at` unspecified. `lag` argument shifts windows left (`lag > 0`)
#'    or right (`lag < 0`). \cr
#'    \if{html}{\figure{incrementalindex.png}{options: width="75\%" alt="Figure: incrementalindex.png"}}
#'    \if{latex}{\figure{incrementalindex.pdf}{options: width=7cm}}
#'  }
#'  \item{**Windows depending on date**}{\cr
#'    If one specifies `idx` this would mean that output windows size might
#'    change in size because of unequally spaced indexes. Fox example 5-period
#'    window is different than 5-element window, because 5-period window might
#'    contain any number of observation (7-day mean is not the same as 7-element
#'    mean)
#'     \cr
#'    \if{html}{\figure{runningdatewindows.png}{options: width="75\%" alt="Figure: runningdatewindows.png"}}
#'    \if{latex}{\figure{runningdatewindows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Window at specific indices**}{\cr
#'    `runner` by default returns vector of the same size as `x` unless one
#'    specifies `at` argument. Each element of `at` is an index on which runner
#'    calculates function - which means that output of the runner is now of
#'    length equal to `at`. Note that one can change index of `x` by specifying
#'    `idx`. Illustration below shows output of `runner` for
#'    `at = c(18, 27, 45, 31)` which gives windows in ranges enclosed in square
#'    brackets. Range for `at = 27` is `[22, 26]` which is not available in
#'    current indices. \cr
#'    \if{html}{\figure{runnerat.png}{options: width="75\%" alt="Figure: runnerat.png"}}
#'    \if{latex}{\figure{runnerat.pdf}{options: width=7cm}}
#'  }
#' }
#' ## Specifying time-intervals
#'  `at` can also be specified as interval of the output defined by
#'  `at = "<increment>"` which results in indices sequence defined by
#'  `seq.POSIXt(min(idx), max(idx), by = "<increment>")`. Increment of sequence
#'  is the same as in \code{\link[base]{seq.POSIXt}} function.
#'  It's worth noting that increment interval can't be more frequent than
#'  interval of `idx` - for `Date` the most frequent time-unit is a `"day"`,
#'  for `POSIXt` a `sec`.
#'
#'  `k` and `lag` can also be specified as using time sequence increment.
#'  Available time units are
#'`"sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"`.
#'  To increment by number of units one can also specify `<number> <unit>s`
#'  for example `lag = "-2 days"`, `k = "5 weeks"`.
#'
#'  Setting `k` and `lag` as a sequence increment can be also a vector can be a
#'  vector which allows to stretch and lag/lead each window freely on in time
#'  (on indices).
#' \cr
#' ## Parallel computing
#'  Beware that executing R call in parallel not always
#'  have the edge over single-thread even if the
#'  `cl <- registerCluster(detectCores())` was specified before.
#'  \cr
#'  Parallel windows are executed in the independent environment, which means
#'  that objects other than function arguments needs to be copied to the
#'  parallel environment using \code{\link[parallel]{clusterExport}}`. For
#'  example using `f = function(x) x + y + z` will result in error as
#'  \code{clusterExport(cl, varlist = c("y", "z"))} needs to be called before.
#'
#' @return vector with aggregated values for each window. Length of output is
#'  the same as `length(x)` or `length(at)` if specified. Type of the output
#'  depends on the output from a function `f`.
#'
#' @md
#' @rdname runner
#' @importFrom methods is
#' @importFrom parallel clusterExport parLapply
#' @export
runner <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
  ) {
  UseMethod("runner", x)
}

#' @rdname runner
#' @examples
#'
#' # runner returns windows as is by default
#' runner(1:10)
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
#'   f = function(x) paste(paste0(x$a, x$b), collapse = "+")
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
#'   xxx = "..."
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
#'        at = c(1, 5, 8))
#'
#' # 5 days mean
#' idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)
#' runner::runner(
#'   x = idx,
#'   k = "5 days",
#'   lag = 1,
#'   idx = Sys.Date() + idx,
#'   f = function(x) mean(x)
#' )
#'
#'# 5 days mean at 4-indices
#' runner::runner(
#'   x = 1:15,
#'   k = 5,
#'   lag = 1,
#'   idx = idx,
#'   at = c(18, 27, 48, 31),
#'   f = mean
#' )
#' @export
runner.default <- function(  #nolint
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at  <- .seq_at(at, idx)
  k   <- .k_by(k, if (length(at > 0)) at else idx, "k")
  lag <- .k_by(lag, if (length(at > 0)) at else idx, "lag")

  w <- window_run(x = x, k = k, lag = lag, idx = idx, at = at, na_pad = na_pad)

  answer <- if (!is.null(cl) && is(cl, "cluster")) {
    parLapply(cl = cl, X = w, fun = f, ...)
  } else {
    lapply(w, function(.this_window)
      if (length(.this_window) == 0) {
        NA
      } else {
        f(.this_window, ...)
      }
    )
  }

  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @examples
#'
#' # runner with data.frame
#' df <- data.frame(
#'   a = 1:13,
#'   b = 1:13 + rnorm(13, sd = 5),
#'   idx = seq(Sys.Date(), Sys.Date() + 365, by = "1 month")
#' )
#' runner(
#'   x = df,
#'   idx = "idx",
#'   at = "6 months",
#'   f = function(x) {
#'     cor(x$a, x$b)
#'   }
#' )
#'
#' # parallel computing
#' library(parallel)
#' data <- data.frame(
#'   a = runif(100),
#'   b = runif(100),
#'   idx = cumsum(sample(rpois(100, 5)))
#' )
#' const <- 0
#' cl <- makeCluster(1)
#' clusterExport(cl, "const", envir = environment())
#'
#' runner(
#'   x = data,
#'   k = 10,
#'   f = function(x) {
#'     cor(x$a, x$b) + const
#'   },
#'   idx = "idx",
#'   cl = cl
#' )
#' stopCluster(cl)
#' @export
runner.data.frame <- function( #nolint
  x,
  f = function(x) x,
  k = attr(x, "k"),
  lag = if (!is.null(attr(x, "lag"))) attr(x, "lag") else integer(1),
  idx = attr(x, "idx"),
  at = attr(x, "at"),
  na_pad = if (!is.null(attr(x, "na_pad"))) attr(x, "na_pad") else FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  # Check if argument is either a colname of x or valid vector of values
  .check_unresolved_difftime(x, k)
  .check_unresolved_difftime(x, lag)
  .check_unresolved_index(x, idx)
  .check_unresolved_at(x, at)

  # if argument is a name of the column in x then column of x is taken
  k   <- .resolve_arg(x, k)
  lag <- .resolve_arg(x, lag)
  idx <- .resolve_arg(x, idx)
  at  <- .resolve_arg(x, at)

  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at  <- .seq_at(at, idx)
  k   <- .k_by(k, if (length(at) > 0) at else idx, "k")
  lag <- .k_by(lag, if (length(at) > 0) at else idx, "lag")

  w <- window_run(
    x = seq_len(nrow(x)),
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )

  answer <- if (!is.null(cl) && is(cl, "cluster")) {
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    parLapply(
      cl = cl,
      X = w,
      fun = function(.this_window_idx) {
        if (length(.this_window_idx) == 0) {
          NA
        } else {
          f(x[.this_window_idx, ], ...)
        }
      }
    )

  } else {
    lapply(w, function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, ], ...)
      }
    })
  }

  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @export
runner.grouped_df <- function(
  x,
  f = function(x) x,
  k = attr(x, "k"),
  lag = if (!is.null(attr(x, "lag"))) attr(x, "lag") else integer(1),
  idx = attr(x, "idx"),
  at = attr(x, "at"),
  na_pad = if (!is.null(attr(x, "na_pad"))) attr(x, "na_pad") else FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  runner.data.frame(
    x = .this_group(x),
    f = f,
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad,
    simplify = simplify,
    cl = cl,
    ...
  )
}

#' @rdname runner
#' @examples
#'
#' # runner with matrix
#' data <- matrix(data = runif(100, 0, 1), nrow = 20, ncol = 5)
#' runner(
#'   x = data,
#'   f = function(x) {
#'     tryCatch(
#'       cor(x),
#'       error = function(e) NA
#'     )
#'  })
#' @export
runner.matrix <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at  <- .seq_at(at, idx)
  k   <- .k_by(k,   if (length(at) > 0) at else idx, "k")
  lag <- .k_by(lag, if (length(at) > 0) at else idx, "lag")

  w <- window_run(
    x = seq_len(nrow(x)),
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )

  answer <- if (!is.null(cl) && is(cl, "cluster"))  {
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    parLapply(
      cl = cl,
      X = w,
      fun = function(.this_window_idx) {
        if (length(.this_window_idx) == 0) {
          NA
        } else {
          f(x[.this_window_idx, , drop = FALSE], ...)
        }
      },
      ...
    )
  } else {
    lapply(
      X = w,
      FUN = function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, , drop = FALSE], ...)
      }
    })
  }
  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @export
runner.xts <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (!identical(idx, integer(0))) {
    warning(
      "'idx' argument has been specified and will mask index
      of the 'xts' object."
    )
  } else {
    idx <- structure(
      .Data = as.vector(attr(x, "index")),
      class = attr(attr(x, "index"), "tclass"),
      tz = attr(attr(x, "index"), "tzone")
    )
  }

  runner.matrix(
    x = x,
    f = f,
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad,
    simplify = simplify,
    cl,
    ...
  )
}
