#' Set window parameters
#'
#' Set window parameters for \link{runner}. This function sets the
#' attributes to \code{x} (only \code{data.frame}) object and saves user effort
#' to specify window parameters in further multiple \link{runner} calls.
#' @inheritParams runner
#' @return x object which \link{runner} can be executed on.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data <- data.frame(
#'  index = c(2, 3, 3, 4, 5, 8, 10, 10, 13, 15),
#'  a = rep(c("a", "b"), each = 5),
#'  b = 1:10
#' )
#'
#' data %>%
#'  group_by(a) %>%
#'  run_by(idx = "index", k = 5) %>%
#'  mutate(
#'    c = runner(
#'      x = .,
#'      f = function(x) {
#'        paste(x$b, collapse = ">")
#'      }
#'    ),
#'    d = runner(
#'      x = .,
#'      f = function(x) {
#'        sum(x$b)
#'      }
#'    )
#'  )
#' }
#' @export
run_by <- function(x, idx, k, lag, na_pad, at) {
  if (!is.data.frame(x)) {
    stop("`run_by` should be used only for `data.frame`. \n
         Use `runner` on x directly.")
  }

  if (!missing(k)) x <- set_run_by_difftime(x, k)
  if (!missing(lag)) x <- set_run_by_difftime(x, lag)
  if (!missing(idx)) x <- set_run_by_index(x, idx)
  if (!missing(at)) x <- set_run_by_index(x, at)
  if (!missing(na_pad)) attr(x, "na_pad") <- na_pad

  return(x)
}

set_run_by_index <- function(x, arg) {
  arg_name <- deparse(substitute(arg))
  attr(x, arg_name) <- if (is.character(arg) &&
                           length(arg) == 1 &&
                           arg %in% names(x)) {
    arg
  } else if (is.numeric(arg) ||
             inherits(arg, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
    arg
  } else {
    stop(
      sprintf(
        "`%s` should be either:
         - column name of `x`
         - vector of type `numeric`, `Date`, `POSIXct` or `POSIXlt`",
        arg_name
      ),
      call. = FALSE
    )
  }
  return(x)
}

set_run_by_difftime <- function(x, arg) {
  arg_name <- deparse(substitute(arg))

  attr(x, arg_name) <- if (is.character(arg)) {
    if (length(arg) == 1 && arg %in% names(x)) {
      arg
    } else if (all(is_datetime_valid(arg))) {
      arg
    } else {
      stop(
        sprintf(
          "`%s` is invalid, should be either:
           - column name of `x`
           - `difftime` class or character describing diffitme (see at argument in `seq.POSIXt`) #nolint
           - `numeric` or `integer` vector",
          arg_name
        ),
        call. = FALSE
      )
    }
  } else if (is.numeric(arg) || is(arg, "difftime")) {
    arg
  } else {
    stop(
      sprintf(
        "`%s` is invalid, should be either:
           - column name of `x`
           - `difftime` class or character describing diffitme (see at argument in `seq.POSIXt`) #nolint
           - `numeric` or `integer` `vector`",
        arg_name
      ),
      call. = FALSE
    )
  }
  return(x)
}
