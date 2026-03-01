#' Apply function on running windows
#'
#' Applies any R function `f` on running (Column/sliding) windows defined by
#' `k`, `lag`, `idx` and `at`.
#'
#' @param x (`vector`, `data.frame`, `matrix`, `xts`, `grouped_df`)\cr
#'  input data.
#'
#' @param k (`integer` or `character`)\cr
#'  Window size. Single value or vector of `length(x)`. Omit for cumulative
#'  windows. Accepts time-interval strings (e.g. `"5 days"`) when `idx` is set.
#'
#' @param lag (`integer` or `character`)\cr
#'  Window shift. Positive shifts back, negative shifts forward. Single value
#'  or vector of `length(x)`. Accepts time-interval strings when `idx` is set.
#'
#' @param idx (`integer`, `Date`, `POSIXt`)\cr
#'  Sorted index of observations. When set, `k` and `lag` refer to index
#'  distance rather than element count. Must be same length as `x`.
#'
#' @param f (`function`)\cr
#'  Function applied to each window. Defaults to identity (`function(x) x`).
#'
#' @param at (`integer`, `Date`, `POSIXt`, `character`)\cr
#'  Indices at which to evaluate windows. Output length equals `length(at)`
#'  instead of `length(x)`. A single time-interval string (e.g. `"month"`)
#'  generates a regular sequence over the range of `idx`.
#'
#' @param na_pad (`logical`)\cr
#'  If `TRUE`, return `NA` for windows that extend beyond the data range.
#'
#' @param simplify (`logical` or `character`)\cr
#'  Simplify result like [base::sapply()]. `TRUE` returns vector/matrix,
#'  `"array"` may return a higher-dimensional array.
#'
#' @param cl (`cluster`) *experimental*\cr
#'  Parallel cluster from [parallel::makeCluster()].
#'
#' @param ... additional arguments passed to `f`.
#'
#' @details
#' ## Window types
#'
#' - **Cumulative** (`k` omitted): window grows from the first element to the
#'   current one, similar to `cumsum`. Each step includes all preceding data.
#'   \cr
#'    \if{html}{\figure{cumulativewindows.png}{options: alt="Figure: cumulativewindows.png"}}
#'    \if{latex}{\figure{cumulativewindows.pdf}{options: width=7cm}}
#'
#' - **Sliding** (constant `k`): fixed-size window slides along the data. The
#'   first `k-1` windows are shorter since there aren't enough preceding
#'   elements. `lag` shifts the window backward (positive) or forward
#'   (negative).
#'   \cr
#'    \if{html}{\figure{incrementalindex.png}{options: alt="Figure: incrementalindex.png"}}
#'    \if{latex}{\figure{incrementalindex.pdf}{options: width=7cm}}
#'
#' - **Index-based** (`idx` set): window covers a range of index values rather
#'   than a fixed number of elements. This is important for unevenly-spaced
#'   data where a 5-day window may contain different numbers of observations
#'   at each step.
#'   \cr
#'    \if{html}{\figure{runningdatewindows.png}{options: alt="Figure: runningdatewindows.png"}}
#'    \if{latex}{\figure{runningdatewindows.pdf}{options: width=7cm}}
#'
#' - **Evaluation at specific points** (`at` set): by default, `runner` returns
#'   one result per element of `x`. Setting `at` restricts output to specific
#'   index positions, so output length equals `length(at)`.
#'   \cr
#'    \if{html}{\figure{runnerat.png}{options: alt="Figure: runnerat.png"}}
#'    \if{latex}{\figure{runnerat.pdf}{options: width=7cm}}
#'
#' ## Time-interval syntax
#'
#' `k`, `lag` and `at` accept time-interval strings (requires `idx` to be set)
#' using the same syntax as the `by` argument in [base::seq.POSIXt()]:
#' `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`, `"month"`, `"quarter"`,
#' `"year"`, or `"<n> <unit>s"` (e.g. `"5 days"`, `"-2 weeks"`).
#' Both `k` and `lag` can also be vectors, allowing different window sizes
#' and shifts at each position.
#'
#' ## Parallel computing
#'
#' Pass a [parallel::makeCluster()] object via `cl` to run windows in
#' parallel. Objects referenced inside `f` (other than its arguments) must
#' be exported with [parallel::clusterExport()] beforehand. Parallel
#' execution adds overhead and is only beneficial for expensive computations.
#'
#' @return vector with aggregated values for each window. Length equals
#'  `length(x)` or `length(at)` if specified. Type depends on `f`.
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
#'   k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
#'   f = function(x) length(unique(x))
#' )
#'
#' # concatenate only on selected windows index
#' runner(letters[1:10],
#'   f = function(x) paste(x, collapse = "-"),
#'   at = c(1, 5, 8)
#' )
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
#' # 5 days mean at 4-indices
#' runner::runner(
#'   x = 1:15,
#'   k = 5,
#'   lag = 1,
#'   idx = idx,
#'   at = c(18, 27, 48, 31),
#'   f = mean
#' )
#' @export
runner.default <- function(
  # nolint
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
    stop("Function doesn't accept NA values in k vector")
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector")
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector")
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at <- .seq_at(at, idx)
  k <- .k_by(k, if (length(at > 0)) at else idx, "k")
  lag <- .k_by(lag, if (length(at > 0)) at else idx, "lag")

  if (!is.null(cl) && is(cl, "cluster")) {
    w <- window_run(x = x, k = k, lag = lag, idx = idx, at = at, na_pad = na_pad)
    answer <- parLapply(cl = cl, X = w, fun = f, ...)
  } else {
    f_wrapped <- if (length(list(...)) > 0) {
      dots <- list(...)
      function(x) do.call(f, c(list(x), dots))
    } else {
      f
    }
    answer <- runner_run(x = x, f = f_wrapped, k = k, lag = lag, idx = idx, at = at, na_pad = na_pad)
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
#'   idx = seq(as.Date("2022-02-22"), as.Date("2023-02-22"), by = "1 month")
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
runner.data.frame <- function(
  # nolint
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
  k <- .resolve_arg(x, k)
  lag <- .resolve_arg(x, lag)
  idx <- .resolve_arg(x, idx)
  at <- .resolve_arg(x, at)

  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector")
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector")
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector")
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at <- .seq_at(at, idx)
  k <- .k_by(k, if (length(at) > 0) at else idx, "k")
  lag <- .k_by(lag, if (length(at) > 0) at else idx, "lag")

  if (!is.null(cl) && is(cl, "cluster")) {
    w <- window_run(
      x = seq_len(nrow(x)),
      k = k,
      lag = lag,
      idx = idx,
      at = at,
      na_pad = na_pad
    )
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    answer <- parLapply(
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
    f_wrapped <- function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, ], ...)
      }
    }
    answer <- runner_run(
      x = seq_len(nrow(x)),
      f = f_wrapped,
      k = k,
      lag = lag,
      idx = idx,
      at = at,
      na_pad = na_pad
    )
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
#'   }
#' )
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
    stop("Function doesn't accept NA values in k vector")
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector")
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector")
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at <- .seq_at(at, idx)
  k <- .k_by(k, if (length(at) > 0) at else idx, "k")
  lag <- .k_by(lag, if (length(at) > 0) at else idx, "lag")

  if (!is.null(cl) && is(cl, "cluster")) {
    w <- window_run(
      x = seq_len(nrow(x)),
      k = k,
      lag = lag,
      idx = idx,
      at = at,
      na_pad = na_pad
    )
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    answer <- parLapply(
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
    f_wrapped <- function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, , drop = FALSE], ...)
      }
    }
    answer <- runner_run(
      x = seq_len(nrow(x)),
      f = f_wrapped,
      k = k,
      lag = lag,
      idx = idx,
      at = at,
      na_pad = na_pad
    )
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
