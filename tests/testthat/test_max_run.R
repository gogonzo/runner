context("Running max")
suppressWarnings(RNGversion("3.5.0"))
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)
k  <- sample(1:15,15, replace=T)
idx <- cumsum(sample(c(1,2,3,4), 15, replace = TRUE))
lag <- sample(0:3, 15, replace = TRUE)

test_that("max_run basic",{
  expect_equal(max_run(x1), runner(x1, f = max))
})

test_that("max_run with na_rm = TRUE", {
  expect_equal(max_run(x2, na_rm = TRUE),
               runner(x2, f = function(x) max(x, na.rm = TRUE)))
})

test_that("max_run with na_rm = FALSE", {
  expect_equal(max_run(x2, na_rm = FALSE),
               runner(x2, f = function(x) max(x, na.rm = FALSE)))
})


test_that("max_run with na_rm=T k=4", {
  expect_equal(max_run(x2, k = 4, na_rm = TRUE),
               runner(x2, k = 4, f = function(x) max(x, na.rm = TRUE)))
})

test_that("max_run with na_rm=F k=4", {
  expect_equal(max_run(x2, k = 4, na_rm = FALSE),
               runner(x2, k = 4, f = function(x) max(x, na.rm = FALSE)))
})

test_that("max_run pads NA's", {
  expect_equal(max_run(x2, k = 5, na_pad = TRUE, na_rm = TRUE),
               c(NA, NA, NA, NA, runner(x2, k = 5, f = function(x) max(x, na.rm = TRUE))[-seq_len(4)]))
})

test_that("varying window", {
  test <- max_run(x2, k = k, na_rm = TRUE)
  comp <- suppressWarnings(runner(x2, k = k, f = function(x) max(x, na.rm = TRUE)))
  comp[!is.finite(comp)] <- NA_real_
  expect_identical(test, comp)
})

test_that("max_run with idx++ same as max_run with windows",{
  expect_identical(max_run(x1, k = 3),
                   max_run(x1, k = 3, idx = seq_len(15)))
  expect_identical(max_run(x1, k = k),
                   max_run(x1, k = k, idx = seq_len(15)))
})

test_that("max_run with idx",{
  expect_identical(
    max_run(x1, k = 4, idx = idx),
    sapply(window_run(x1, k = 4, idx = idx), max)
  )

  expect_identical(
    max_run(x1, k = k, idx = idx),
    sapply(window_run(x1, k = k, idx = idx), max)
  )

  expect_equal(
    max_run(x1, k = 4, lag = 3, idx = idx),
    runner(x1, k = 4, lag = 3, idx = idx, f = max)
  )

  expect_equal(
    max_run(x1, k = k, lag = 3, idx = idx),
    runner(x1,  k = k, lag = 3, idx = idx, f = max)
  )

  expect_equal(
    max_run(x1, k = 5, lag = lag, idx = idx),
    runner(x1,  k = 5, lag = lag, idx = idx, f = max)
  )

  expect_equal(
    max_run(x1, k = k, lag = lag, idx = idx),
    runner(x1,  k = k, lag = lag, idx = idx, f = max)
  )
})

test_that("Errors", {
  expect_error(max_run(x1, k = (1:9)), "length of k and length of x differs")
  expect_error(max_run(x1, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(max_run(x1, lag = (1:9)), "length of lag and length of x differs")
  expect_error(max_run(x1, lag = c(NA, k[-1])), "Function doesn't accept NA values in lag vector")
  expect_warning(max_run(x1, lag = 15), "lag value is greater than length of x")

  expect_error(max_run(x1, idx = (1:9)), "length of idx and length of x differs")
  expect_error(max_run(x1, idx = c(NA, 1:14)), "Function doesn't accept NA values in idx vector")
})
