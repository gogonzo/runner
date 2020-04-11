#' Apply running function
#'
#' Applies custom function on running windows.
#' @param x (`vector`, `data.frame`, `matrix`)\cr
#'  Input in runner custom function `f`.
#'
#' @param k (`integer` vector or single value)\cr
#'  Denoting size of the running window. If `k` is a single value then window
#'  size is constant for all elements, otherwise if `length(k) == length(x)`
#'  different window size for each element. One can also specify `k` in the same
#'  way as by in \code{\link[base]{seq.POSIXt}}. More in details.
#'
#' @param lag (`integer` vector or single value)\cr
#'  Denoting window lag. If `lag` is a single value then window lag is constant
#'  for all elements, otherwise if `length(lag) == length(x)` different window
#'  size for each element. Negative value shifts window forward. One can also
#'  specify `lag` in the same way as by in \code{\link[base]{seq.POSIXt}}.
#'  More in details.
#'
#' @param idx (`integer`, `Date`, `POSIXt`)\cr
#'  Optional integer vector containing sorted (ascending) index of observation.
#'  By default `idx` is index incremented by one. User can provide index with
#'  varying increment and with duplicated values. If specified then `k` and `lag`
#'  are depending on `idx`. Length of `idx` have to be equal of length `x`.
#'
#' @param f (`function`)\cr
#' Applied on windows created from `x`. This function is meant to summarize
#' windows and create single element for each window, but one can also specify
#' function which return multiple elements (runner output will be a list).
#' By default runner returns windows as is (`f = function(x)`).
#'
#' @param at (`integer`, `Date`, `POSIXt`, `character` vector)\cr
#'  Vector of any size and any value defining output data points. Values of the
#'  vector defines the indexes which data is computed at. Can be also `POSIXt`
#'  sequence increment \code{\link[base]{seq.POSIXt}}. More in details.
#'
#' @param na_pad (`logical` single value)\cr
#'  Whether incomplete window should return `NA` (if `na_pad = TRUE`)
#'  Incomplete window is when some parts of the window are out of range.
#'
#' @param type (`character` single value)\cr
#'  output type (`"auto"`, `"logical"`, `"numeric"`, `"integer"`, `"character"`).
#'  `runner` by default guess type automatically. In case of failure of `"auto"`
#'  please specify desired type.
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
#'    contain any number of observation (7-day mean is not the same as 7-element mean)
#'     \cr
#'    \if{html}{\figure{runningdatewindows.png}{options: width="75\%" alt="Figure: runningdatewindows.png"}}
#'    \if{latex}{\figure{runningdatewindows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Window at specific indices**}{\cr
#'    `runner` by default returns vector of the same size as `x` unless one specifies
#'    `at` argument. Each element of `at` is an index on which runner calculates function -
#'    which means that output of the runner is now of length equal to `at`. Note
#'    that one can change index of `x` by specifying `idx`.
#'    Illustration below shows output of `runner` for `at = c(18, 27, 45, 31)`
#'    which gives windows in ranges enclosed in square brackets. Range for `at = 27` is
#'    `[22, 26]` which is not available in current indices. \cr
#'    \if{html}{\figure{runnerat.png}{options: width="75\%" alt="Figure: runnerat.png"}}
#'    \if{latex}{\figure{runnerat.pdf}{options: width=7cm}}
#'    \cr
#'    `at` can also be specified as interval of the output defined by `at = "<increment>"`
#'    which results in output on following indices
#'    `seq.POSIXt(min(idx), max(idx), by = "<increment>")`. Increment of sequence is the
#'    same as in \code{\link[base]{seq.POSIXt}} function.
#'    It's worth noting that increment interval can't be more frequent than
#'    interval of `idx` - for `Date` the most frequent time-unit is a `"day"`,
#'    for `POSIXt` a `sec`.
#'
#'    `k` and `lag` can also be specified as using time sequence increment. Available
#'    time units are `"sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"`.
#'    To increment by number of units one can also specify `<number> <unit>s`
#'    for example `lag = "-2 days"`, `k = "5 weeks"`.
#'  }
#' }
#' Above is not enough since `k` and `lag` can be a vector which allows to
#' stretch and lag/lead each window freely on in time (on indices).
#'
#' @return vector with aggregated values for each window. Length of output is the
#'  same as `length(x)` or `length(at)` if specified. Type of the output
#'  is taken from `type` argument.
#'
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
#' @md
#' @importFrom methods is
#' @export
runner <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type = "auto",
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


  at <- seq_by(at, idx)
  k <- k_by(k, if (length(at > 0)) at else idx, "k")
  lag <- k_by(lag, if (length(at > 0)) at else idx, "lag")

  w <- window_run(
    x = if (is.data.frame(x) || is.matrix(x)) {
      seq_len(nrow(x))
    } else {
      x
    },
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

#' Creates sequence for at as time-unit-interval
#'
#' Creates sequence for at as time-unit-interval
#' @param at object from runner
#' @param idx object from runner
seq_by <- function(at, idx) {
  if ((is.character(at) &&
       length(at) == 1)) {

    if (length(idx) == 0) {
      stop(
        sprintf("`idx` can't be empty while specifying at as time interval")
      )
    }

    if (inherits(idx, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      at <- if (grepl("^-", at)) {
        seq(max(idx), min(idx), by = at)
      } else {
        seq(min(idx), max(idx), by = at)
      }
    } else {
      stop("To specify at as time interval character `idx` can't be empty")
    }
  }
  return(at)
}

#' Converts k and lag from time-unit-interval to int
#'
#' Converts k and lag from time-unit-interval to int
#' @param k object from runner
#' @param idx object from runner
#' @param param name of the parameter to be printed in error message
#' @examples
#' k <-  "1 month"
#' idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
#' k_difftime <- runner:::k_by(k, idx, param = "k")
#' idx - k_difftime
k_by <- function(k, idx, param) {
  if (is.character(k)) {
    k <- if (param == "k") {
      reformat_k(k, only_positive = TRUE)
    } else {
      reformat_k(k, only_positive = FALSE)
    }

    from <- if (length(idx) == length(k) && length(k) != 1) {
      mapply(
        FUN = function(x, y) {
          seq(x, by = y, length.out = 2)[2]
        },
        x = idx,
        y = k
      )
    } else if (length(k) == 1) {
      if (length(idx) == 0) {
        stop(
          sprintf("`idx` can't be empty while specifying %s='%s'", param, k)
        )
      }

      vapply(
        idx,
        function(x) seq(x, by = k, length.out = 2)[2],
        FUN.VALUE = double(1)
      )
    }

    return(as.numeric(idx) - from)
  }

  return(k)
}

#' Formats time-unit-interval to valid for runner
#'
#' Formats time-unit-interval to valid for runner
#' @param k (k or lag) object from runner to be formatted
#' @param only_positive for k is TRUE, for lag is FALSE
#' @examples
#' runner:::reformat_k("1 days")
#' runner:::reformat_k("day")
#' runner:::reformat_k("10 days")
#' runner:::reformat_k("-10 days", only_positive = FALSE)
#' runner:::reformat_k(c("-10 days", "2 months"), only_positive = FALSE)
reformat_k <- function(k, only_positive = TRUE) {
  if (only_positive && any(grepl("^-", k))) {
    stop("k can't be negative")
  }

  k[grepl("^[a-zA-Z]", k)] <- sprintf("1 %ss", k[grepl("^[a-zA-Z]", k)])
  positive <- grepl("^[^-]", k)
  negative <- grepl("^-", k)

  k[positive] <- sprintf("-%s", k[positive])
  k[negative] <- gsub("^-", "", k[negative])

  return(k)
}
