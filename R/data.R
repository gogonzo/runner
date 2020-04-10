#' Dummy data.
#'
#' A dataset containing dummy time-serie in minute interval.
#' @format A data frame with 100000 rows and 2 columns
#' \describe{
#'   \item{min}{minute - irregulary spaced}
#'   \item{val1}{random walk data}
#'   \item{val2}{random walk data - 2 * val1 + epsilon}
#'   \item{val3}{random walk data - -1.5 * val1 + 2epsilon}
#'   \item{val4}{random walk data - 0.7 * val2 + epsilon}
#' }
#' @source internal
#' @rdname dummy_data
#' @examples
#' dummy_min
"dummy_min"


#' Dummy data.
#'
#' A dataset containing dummy time-serie in hourly interval.
#' @format A data frame with 10000 rows and 2 columns (subset of `dummy_min`)
#' @source internal
#' @rdname dummy_data
"dummy_hour"

#' Dummy data.
#'
#' A dataset containing dummy time-serie in daily interval.
#' @format A data frame with 1000 rows and 2 columns (subset of `dummy_min`)
#' @source internal
#' @rdname dummy_data
"dummy_day"
