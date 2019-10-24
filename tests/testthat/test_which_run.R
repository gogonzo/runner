context("Running which")
set.seed(11)
x1 <- sample(c(TRUE, FALSE, NA), 20, replace = TRUE)
x2 <- sample(c(TRUE, FALSE, NA), 20, replace = TRUE)
k <- sample(seq_len(20), 20, replace = TRUE)
idx <- cumsum(rpois(20, 4))
lag <- sample(seq(-5, 5), 20, replace = TRUE)
which_test <- function(x, arg_which, na_rm, i, k, lag = 0) {
  x <- if (!na_rm) {
    if (arg_which == "last") {
      ifelse(
        tail(which(x), 1) > tail(c(0, which(is.na(x))), 1),
        tail(which(x), 1), NA
      )
    } else if (arg_which == "first") {
      ifelse(
        head(which(x), 1) > head(c(0, which(is.na(x))), 1),
        head(which(x), 1), NA
      )
    }
  } else {
    if (arg_which == "last") {
      tail(which(x), 1)
    } else if (arg_which == "first") {
      head(which(x), 1)
    }
  }
  if(length(x) == 0) {
    x <- NA_integer_
  }
  if (!missing(k)) {
    if (!missing(i)) {
      return(as.integer(x + (i - 1) - k - lag + 1))
    } else {
      return(as.integer(x - k - lag + 1))
    }
  } else {
    if (!missing(i)) {
      return(as.integer(x + (i - 1) - lag))
    } else {
      return(as.integer(x - lag))
    }
  }

}

test_that("which_run first and last", {
  expect_identical(
    which_run(x1),
    sapply(window_run(x1), function(x) which_test(x, "last", TRUE))
  )

  expect_identical(
    which_run(x1, which = "first"),
    sapply(window_run(x1), function(x) which_test(x, "first", TRUE))
  )

  expect_identical(
    which_run(x1, which = "last"),
    sapply(window_run(x1), function(x) which_test(x, "last", TRUE))
  )
})

test_that("which with na_rm = FALSE", {
  expect_identical(
    which_run(x2, na_rm = FALSE, which = "last"),
    sapply(window_run(x2), function(x) which_test(x, "last", FALSE))
  )
  expect_identical(
    which_run(x2, na_rm = FALSE, which = "first"),
    sapply(window_run(x2), function(x) which_test(x, "first", FALSE))
  )
})

test_that("which with na_rm = TRUE k = 4 + idx", {
  expect_identical(
    which_run(x2, na_rm = TRUE, which = "first"),
    sapply(window_run(x2), function(x) which_test(x, "first", TRUE))
  )

  expect_identical(
    which_run(x2, na_rm = TRUE, which = "last"),
    sapply(window_run(x2), function(x) which_test(x, "last", TRUE))
  )

  expect_identical(
    which_run(x2, na_rm = TRUE, which = "first", idx = idx),
    sapply(window_run(x2, idx = idx), function(x) which_test(x, "first", TRUE))
  )

  expect_identical(
    which_run(x2, na_rm = TRUE, which = "last", idx = idx),
    sapply(seq_along(x2), function(i) which_test(window_run(x2, idx = idx)[[i]], arg_which = "last", na_rm = TRUE))
  )
})

test_that("which with na_rm = FALSE k = 4", {
  expect_identical(
    which_run(x2, na_rm = FALSE, which = "first", idx = idx),
    sapply(window_run(x2, idx = idx), function(x) which_test(x, "first", FALSE))
  )

  expect_identical(
    which_run(x2, na_rm = FALSE, which = "last", idx = idx),
    sapply(window_run(x2, idx = idx), function(x) which_test(x, "last", FALSE))
  )
})

test_that("which for indexed window",{
  x <- c(NA,F, T, NA, F, F, T, T, NA, T, F , T)
  i <- c(1, 2, 3, 3,  3, 4, 6, 8, 9,  9, 13, 13)
  k <- sample(1:12,12,replace=T)

  expect_identical(
    which_run(x, which = "last", idx = i, k = 3, na_rm = FALSE),
    as.integer(c(NA, NA, 3, NA, NA, NA, 7, 8, NA, 10, NA, 12))
  )
  expect_identical(
    which_run(x, which = "first", idx = i, k = 2, na_rm = FALSE),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 8, NA, 12))
  )
  expect_identical(
    which_run(x, which = "last", idx = i, k = 2, na_rm = TRUE),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 10, NA, 12))
  )

})

test_that("Errors", {

  expect_error(which_run(x1, k = (1:9)), "length of k and length of x differs")
  expect_error(which_run(x1, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(which_run(x1, lag = (1:9)), "length of lag and length of x differs")
  expect_error(which_run(x1, lag = c(NA, k[-1])), "Function doesn't accept NA values in lag vector")
  expect_warning(which_run(x1, lag = 20), "lag value is greater than length of x")

  expect_error(which_run(x1, idx = (1:9)), "length of idx and length of x differs")
  expect_error(which_run(x1, idx = c(NA, 1:19)), "Function doesn't accept NA values in idx vector")

  expect_error(which_run(x1, which = "test"), "which value should be either")
})

