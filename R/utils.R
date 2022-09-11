#' Validate date time character
#'
#' Checks if the character is a valid date time string
#' @param (`character`) can be anything but suppose to be a character.
#' @return `logical(1)` denoting if all elements of the character vectors are valid
#' @keywords internal
.is_datetime_valid <- function(x) {
  is.character(x) &&
    all(
      grepl(x = x, "^(sec|min|hour|day|DSTday|week|month|quarter|year)$") |
        grepl(x = x, "^-*[0-9]+ (sec|min|hour|day|DSTday|week|month|quarter|year)s")
    )
}


#' Converts k and lag from time-unit-interval to int
#'
#' Converts k and lag from time-unit-interval to int
#' @inheritParams runner
#' @param param name of the parameter to be printed in error message
#' @examples
#' k <-  "1 month"
#' idx <- seq(
#'   as.POSIXct("2019-01-01 03:02:01"),
#'   as.POSIXct("2020-01-01 03:02:01"),
#'   by = "month"
#' )
#' k_difftime <- runner:::.k_by(k, idx, param = "k")
#' idx - k_difftime
#' @keywords internal
.k_by <- function(k, idx, param) {
  if (is.character(k)) {
    k <- if (param == "k") {
      .reformat_k(k, only_positive = TRUE)
    } else {
      .reformat_k(k, only_positive = FALSE)
    }

    from <- if (length(k) != 1) {
      if (length(idx) == 0) {
        stop(
          sprintf(
            "`idx` can't be empty while specifying `%s` as time interval",
            param
          )
        )
      }

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
          sprintf(
            "`idx` can't be empty while specifying `%s` as time interval",
            param
          )
        )
      }

      vapply(
        idx,
        function(x) seq(x, by = k, length.out = 2)[2],
        FUN.VALUE = double(1)
      )
    }

    return(as.integer(idx) - from)

  } else if (is(k, "difftime")) {
    k <- if (param == "k") {
      if (any(k < 0)) {
        stop("`k` can't be negative.")
      }
      abs(k)
    } else {
      k
    }

    if (length(idx) == 0) {
      stop(
        sprintf("`idx` can't be empty while specifying %s as difftime", param)
      )
    }
    from <- idx - k
    k <- as.integer(idx) - as.integer(from)
  }

  return(k)
}



#' Formats time-unit-interval to valid for runner
#'
#' Formats time-unit-interval to valid for runner. User specifies \code{k} as
#' positive number but this means that this interval needs to be substracted
#' from \code{idx} - because windows length extends window backwards in time.
#' The same situation for lag.
#' @param k (k or lag) object from runner to be formatted
#' @param only_positive for \code{k} is \code{TRUE},
#'   for \code{lag} is \code{FALSE}
#' @examples
#' runner:::.reformat_k("1 days")
#' runner:::.reformat_k("day")
#' runner:::.reformat_k("10 days")
#' runner:::.reformat_k("-10 days", only_positive = FALSE)
#' runner:::.reformat_k(c("-10 days", "2 months"), only_positive = FALSE)
#' @keywords internal
.reformat_k <- function(k, only_positive = TRUE) {
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


#' Creates sequence for at as time-unit-interval
#'
#' Creates sequence for at as time-unit-interval
#' @param at object from runner
#' @param idx object from runner
#' @keywords internal
.seq_at <- function(at, idx) { # nolint
  if (length(at) == 1 &&
      (
        (is.character(at) && .is_datetime_valid(at)) ||
        is(at, "difftime")
      )
    ) {

    if (length(idx) == 0) {
      stop(
        sprintf("`idx` can't be empty while specifying `at` as time interval")
      )
    }


    if (inherits(idx, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      at <- if ((is.character(at) && grepl("^-", at)) ||
                (is(at, "difftime") && at < 0)) {
        seq(max(idx), min(idx), by = at)
      } else {
        seq(min(idx), max(idx), by = at)
      }
    }
  }
  return(at)
}

#' Resolves at argument
#'
#' Resolves argument passed to the `runner` -
#' checks if the argument has a valid value. If argument is a single character
#' matching column name in the `x` then is replaced with the value `x[[arg]]`
#' @param x (`data.frame`)
#' @inheritParams runner
#' @return resolved `at`
#' @keywords internal
.resolve_arg <- function(x, arg) {
  if (length(arg) > 0) {
    if (length(arg) == 1 && is.character(arg) && arg %in% names(x)) {
      x[[arg]]
    } else {
      arg
    }
  } else {
    integer(0)
  }
}

#' Resolves at argument
#'
#' Resolves `at` argument passed to the `runner` -
#' checks if the argument has a valid value. If argument is a single character
#' matching column name in the `x` then is replaced with the value `x[[at]]`
#' @param x (`data.frame`)
#' @inheritParams runner
#' @return resolved `at`
#' @keywords internal
.check_unresolved_at <- function(x, at) { #nolint
  arg_name <- deparse(substitute(at))

  if (length(at) > 0) {
    if (length(at) == 1 && is.character(at) && at %in% names(x)) {
      at <- x[[at]]
    }
    if (length(at) == 1 && .is_datetime_valid(at)) {
      NULL
    } else if (inherits(at, c("numeric", "integer", "Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      NULL
    } else {
     stop(
        sprintf(
          "`%s` is invalid, should be either:
          - `numeric`, `Date`, `POSIXct` or `POSIXlt` vector of any length.
          - `character(1)` value describing dates sequence step as in `by` argument of `seq.POSIXct`.
          - `character(1)` being a column name of `x` matching above criteria.",
          arg_name
        ),
        call. = FALSE
      )
    }
  }
}

#' Resolves time difference argument
#'
#' Resolves `at` argument passed to the `runner` (`k` or `lag`)
#' checks if the argument has a valid value. If argument is a single character
#' matching column name in the `x` then is replaced with the value `x[[idx]]`
#' @param x (`data.frame`)
#' @inheritParams runner
#' @return resolved `idx`
#' @keywords internal
.check_unresolved_difftime <- function(x, k) { #nolint
  arg_name <- deparse(substitute(k))
  if (length(k) > 0) {
    if (length(k) == 1 && is.character(k) && k %in% names(x)) {
      k <- x[[k]]
    }
    if (length(k) %in% c(1, nrow(x)) && all(.is_datetime_valid(k))) {
      NULL
    } else if (length(k) %in% c(1, nrow(x)) && inherits(k, c("numeric", "integer", "difftime"))) {
      NULL
    } else {
      stop(
        sprintf(
          "`%s` is invalid, should be either:
          - `numeric`, `integer` or `difftime` of length equal to `1` or `nrow(x))`
          - `character(1)` being a column name of `x` matching above criteria.",
          arg_name
        ),
        call. = FALSE
      )
    }
  }
}

#' Resolves index argument
#'
#' Resolves `at` argument passed to the `runner` -
#' checks if the argument has a valid value. If argument is a single character
#' matching column name in the `x` then is replaced with the value `x[[idx]]`
#' @param x (`data.frame`)
#' @inheritParams runner
#' @return resolved `idx`
#' @keywords internal
.check_unresolved_index <- function(x, idx) { #nolint
  arg_name <- deparse(substitute(idx))

  if (length(idx) > 0) {
    if (is.character(idx) && length(idx) == 1 && idx %in% names(x)) {
      idx <- x[[idx]]
    }
    if (length(idx) == nrow(x) &&
        inherits(idx, c("numeric", "integer", "Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      NULL
    } else {
      stop(
        sprintf(
          "`%s` is invalid, should be a vector:
          - with type being a `numeric`, `Date`, `POSIXct` or `POSIXlt` of length equal to `nrow(x)`
          - `character(1)` being a column name of `x` matching above criteria.",
          arg_name
        ),
        call. = FALSE
      )
    }
  }
}

#' Access group data in mutate
#'
#' Access group data in `dplyr::mutate` after
#' `dplyr::group_by`.
#' Function created because data available in `dplyr::group_by %>% mutate`
#' scheme is not filtered by group - in mutate function `.` is still initial
#' dataset. This function creates `data.frame` using `dplyr::groups`
#' information.
#' @param x (`data.frame`)\cr
#'   object which can be `grouped_df` in special case.
#' @md
#' @return data.frame filtered by current `dplyr::groups()`
#' @keywords internal
.this_group <- function(x) {
  attrs <- attributes(x)
  attrs <- attrs[names(attrs) != "row.names"]

  new_env <- new.env(parent = parent.frame(n = 2)$.top_env)
  df_call <- as.call(
    append(
      as.name("data.frame"),
      lapply(names(x), as.name)
    )
  )

  x <- eval(df_call, envir = new_env)
  for (i in seq_along(attrs)) {
    attr(x, names(attrs)[i]) <- attrs[[i]]
  }
  return(x)
}
