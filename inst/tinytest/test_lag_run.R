set.seed(1)
x <- 1L:30L
x2 <- as.numeric(1:30)
x3 <- sample(letters, 30, replace = TRUE)
x4 <- sample(c(TRUE, FALSE), 30, replace = TRUE)

lag  <- sample(-10:10, 30, replace = TRUE)
idx <- cumsum(sample(seq_len(5), 30, replace = TRUE))
lag_run2 <- function(x, lag = 1, idx = seq_along(x), nearest = FALSE) {
  fun <- function(x, k, idx, nearest, i) {
    res <- if (nearest) {
      window <- if (k > 0) {
        x[idx %in% seq(idx[i] - k, idx[i] - 1, by = 1)]
      } else if (k < 0) {
        x[idx %in% seq(idx[i] + 1, idx[i] - k, by = 1)]
      } else {
        return(x[i]);
      }
      if (length(window) > 0) {
        if (k > 0) window[1] else rev(window)[1]
      } else {
        NA
      }
    } else if (!nearest) {
      window <- if (k != 0) {
        x[idx == (idx[i] - k)]
      } else {
        return(x[i]);
      }
      res <- if (length(window) > 0) {
        if (k > 0) window[1] else rev(window)[1]
      } else {
        NA
      }
    }
    return(res)
  }
  sapply(seq_along(x), function(i) {
    if (length(lag) == 1) {
      fun(x, lag[1], idx, nearest, i)
    } else {
      fun(x, lag[i], idx, nearest, i)
    }

  })
}

# lag_run basic - different types ------

expect_identical(
  lag_run(x),
  c(NA, x[-30])
)

expect_identical(
  lag_run(as.character(x)),
  as.character(c(NA_character_, x[-30]))
)

expect_identical(
  lag_run(as.factor(letters)),
  c(NA_integer_, 1:25)
)

expect_identical(
  lag_run(as.integer(round(x))),
  as.integer(round(c(NA_integer_, x[-30])))
)

expect_identical(
  lag_run(as.complex(x)),
  as.complex(c(NA_complex_, x[-30]))
)

expect_identical(
  lag_run(x > 15),
  c(NA, x[-30] > 15)
)
# lag_run constant window" ---------
expect_identical(lag_run(x, lag = 3), c(rep(NA, 3), head(x, -3)))

expect_identical(lag_run(x2, lag = 3), c(rep(NA, 3), head(x2, -3)))

expect_identical(lag_run(x3, lag = 3), c(rep(NA, 3), head(x3, -3)))

expect_identical(lag_run(x4, lag = 3), c(rep(NA, 3), head(x4, -3)))

# lag_run moving window" ---------
expect_identical(lag_run(x, lag = lag), lag_run2(x, lag = lag))
expect_identical(lag_run(x, lag = lag, nearest = FALSE),
                 lag_run2(x, lag = lag, nearest = FALSE))

expect_identical(lag_run(x2, lag = lag), lag_run2(x2, lag = lag))
expect_identical(lag_run(x2, lag = lag, nearest = FALSE),
                 lag_run2(x2, lag = lag, nearest = FALSE))

expect_identical(lag_run(x3, lag = lag), lag_run2(x3, lag = lag))
expect_identical(lag_run(x3, lag = lag, nearest = FALSE),
                 lag_run2(x3, lag = lag, nearest = FALSE))

expect_identical(lag_run(x4, lag = lag), lag_run2(x4, lag = lag))
expect_identical(lag_run(x4, lag = lag, nearest = FALSE),
                 lag_run2(x4, lag = lag, nearest = FALSE))

# lag_run date idx window (nearest end)" -------
expect_identical(lag_run(x, lag = 3, idx = idx, nearest = TRUE),
                 lag_run2(x, lag = 3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x, lag = 3, idx = idx, nearest = FALSE),
                 lag_run2(x, lag = 3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x, lag = -3, idx = idx, nearest = TRUE),
                 lag_run2(x, lag = -3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x, lag = -3, idx = idx, nearest = FALSE),
                 lag_run2(x, lag = -3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x, lag = lag, idx = idx, nearest = TRUE),
                 lag_run2(x, lag = lag, idx = idx, nearest = TRUE))

expect_identical(lag_run(x, lag = lag, idx = idx, nearest = FALSE),
                 lag_run2(x, lag = lag, idx = idx, nearest = FALSE))



expect_identical(lag_run(x2, lag = 3, idx = idx, nearest = TRUE),
                 lag_run2(x2, lag = 3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x2, lag = 3, idx = idx, nearest = FALSE),
                 lag_run2(x2, lag = 3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x2, lag = -3, idx = idx, nearest = TRUE),
                 lag_run2(x2, lag = -3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x2, lag = -3, idx = idx, nearest = FALSE),
                 lag_run2(x2, lag = -3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x2, lag = lag, idx = idx, nearest = TRUE),
                 lag_run2(x2, lag = lag, idx = idx, nearest = TRUE))

expect_identical(lag_run(x2, lag = lag, idx = idx, nearest = FALSE),
                 lag_run2(x2, lag = lag, idx = idx, nearest = FALSE))


expect_identical(lag_run(x3, lag = 3, idx = idx, nearest = TRUE),
                 lag_run2(x3, lag = 3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x3, lag = 3, idx = idx, nearest = FALSE),
                 lag_run2(x3, lag = 3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x3, lag = -3, idx = idx, nearest = TRUE),
                 lag_run2(x3, lag = -3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x3, lag = -3, idx = idx, nearest = FALSE),
                 lag_run2(x3, lag = -3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x3, lag = lag, idx = idx, nearest = TRUE),
                 lag_run2(x3, lag = lag, idx = idx, nearest = TRUE))

expect_identical(lag_run(x3, lag = lag, idx = idx, nearest = FALSE),
                 lag_run2(x3, lag = lag, idx = idx, nearest = FALSE))


expect_identical(lag_run(x4, lag = 3, idx = idx, nearest = TRUE),
                 lag_run2(x4, lag = 3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x4, lag = 3, idx = idx, nearest = FALSE),
                 lag_run2(x4, lag = 3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x4, lag = -3, idx = idx, nearest = TRUE),
                 lag_run2(x4, lag = -3, idx = idx, nearest = TRUE))

expect_identical(lag_run(x4, lag = -3, idx = idx, nearest = FALSE),
                 lag_run2(x4, lag = -3, idx = idx, nearest = FALSE))

expect_identical(lag_run(x4, lag = lag, idx = idx, nearest = TRUE),
                 lag_run2(x4, lag = lag, idx = idx, nearest = TRUE))

expect_identical(lag_run(x4, lag = lag, idx = idx, nearest = FALSE),
                 lag_run2(x4, lag = lag, idx = idx, nearest = FALSE))

# errors ------
expect_error(lag_run(x, lag = (1:9)),
             "length of lag and length of x differs")
expect_error(min_run(x, lag = c(NA, lag[-1])),
             "Function doesn't accept NA values in lag vector")

expect_error(lag_run(x, idx = (1:9)),
             "length of idx and length of x differs")
expect_error(lag_run(x, idx = c(NA, 1:29)),
             "Function doesn't accept NA values in idx vector")
expect_error(lag_run(x, idx = sample(1:30)),
             "idx have to be in ascending order")
