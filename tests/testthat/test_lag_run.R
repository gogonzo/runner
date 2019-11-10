context("Running lag")
set.seed(1)
x <- 1:30
k  <- sample(-10:10, 30, replace = TRUE)
idx <- cumsum(sample(seq_len(5), 30, replace = TRUE))
lag_run2 <- function(x, k = 1, idx = seq_along(x), nearest = FALSE) {
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
    if (length(k) == 1) {
      fun(x, k[1], idx, nearest, i)
    } else {
      fun(x, k[i], idx, nearest, i)
    }

  })
}

test_that("lag_run basic - different types", {
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
})

test_that("lag_run constant window", {
  expect_identical(lag_run(x, k = 3), c(rep(NA, 3), head(x, -3)))
})

test_that("lag_run moving window", {
  expect_identical(lag_run(x, k = k), lag_run2(x, k = k))
  expect_identical(lag_run(x, k = k, nearest = FALSE),
                   lag_run2(x, k = k, nearest = FALSE))
})

test_that("lag_run date idx window (nearest end)", {
  expect_identical(lag_run(x, k = 3, idx = idx, nearest = TRUE),
                   lag_run2(x, k = 3, idx = idx, nearest = TRUE))

  expect_identical(lag_run(x, k = 3, idx = idx, nearest = FALSE),
                   lag_run2(x, k = 3, idx = idx, nearest = FALSE))

  expect_identical(lag_run(x, k = -3, idx = idx, nearest = TRUE),
                   lag_run2(x, k = -3, idx = idx, nearest = TRUE))

  expect_identical(lag_run(x, k = -3, idx = idx, nearest = FALSE),
                   lag_run2(x, k = -3, idx = idx, nearest = FALSE))

  expect_identical(lag_run(x, k = k, idx = idx, nearest = TRUE),
                   lag_run2(x, k = k, idx = idx, nearest = TRUE))

  expect_identical(lag_run(x, k = k, idx = idx, nearest = FALSE),
                   lag_run2(x, k = k, idx = idx, nearest = FALSE))
})

test_that("Errors", {
  expect_error(lag_run(x, k = (1:9)), "length of k and length of x differs")
  expect_error(min_run(x, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(lag_run(x, idx = (1:9)), "length of idx and length of x differs")
  expect_error(lag_run(x, idx = c(NA, 1:29)), "Function doesn't accept NA values in idx vector")
})

