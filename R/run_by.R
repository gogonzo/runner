#' Set window parameters
#'
#' Set window parameters for [runner()]. This function sets the
#' attributes to `x` (only `data.frame`) object and saves user effort
#' to specify window parameters in further multiple [runner()] calls.
#' @inheritParams runner
#' @return x object which [runner()] can be executed on.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data <- data.frame(
#'   index = c(2, 3, 3, 4, 5, 8, 10, 10, 13, 15),
#'   a = rep(c("a", "b"), each = 5),
#'   b = 1:10
#' )
#'
#' data %>%
#'   group_by(a) %>%
#'   run_by(idx = "index", k = 5) %>%
#'   mutate(
#'     c = runner(
#'       x = .,
#'       f = function(x) {
#'         paste(x$b, collapse = ">")
#'       }
#'     ),
#'     d = runner(
#'       x = .,
#'       f = function(x) {
#'         sum(x$b)
#'       }
#'     )
#'   )
#' }
#' @export
run_by <- function(x, idx, k, lag, na_pad, at) {
  if (!is.data.frame(x)) {
    stop("`run_by` should be used only with `data.frame`. \n Please use `runner` on `x` directly.")
  }

  if (!missing(k)) {
    .check_unresolved_difftime(x, k)
    attr(x, "k") <- k
  }
  if (!missing(lag)) {
    .check_unresolved_difftime(x, lag)
    attr(x, "lag") <- lag
  }
  if (!missing(idx)) {
    .check_unresolved_index(x, idx)
    attr(x, "idx") <- idx
  }
  if (!missing(at)) {
    .check_unresolved_at(x, at)
    attr(x, "at") <- at
  }
  if (!missing(na_pad)) attr(x, "na_pad") <- na_pad

  return(x)
}
