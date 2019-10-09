context("Running mean")
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)
k  <- sample(1:15,15, replace=T)
idx <- cumsum(sample(c(1,2,3,4), 15, replace = TRUE))
lag <- sample(0:3, 15, replace = TRUE)

test_that("mean_run basic",{
  expect_identical(
    mean_run(x1),
    runner(x1, f = mean)
  )
})

test_that("mean_run with na_rm = TRUE", {
  expect_identical(
    mean_run(x2, na_rm = TRUE),
    runner(x2, f = function(x) mean(x, na.rm = TRUE))
  )
})

test_that("mean_run with na_rm = FALSE na_fill = TRUE", {
  expect_identical(
    mean_run(x2, na_rm = FALSE),
    runner(x2, f = function(x) mean(x, na.rm = FALSE))
  )
})


test_that("mean_run with na_rm = FALSE k = 4", {
  expect_identical(
    mean_run(x2, k = 4, na_rm = FALSE),
    runner(x2, k = 4, f = function(x) mean(x, na.rm = FALSE))
  )
})

test_that("mean_run with na_rm = TRUE k = 4", {
  expect_identical(
    mean_run(x2, k = 4, na_rm = TRUE),
    runner(x2, k = 4, f = function(x) mean(x, na.rm = TRUE))
  )
})

test_that("mean_run with idx++ same as mean_run with windows",{
  expect_identical(mean_run(x1, k = 3),
                   mean_run(x1, k = 3, idx = 0:14))

  expect_identical(mean_run(x1, k = k),
                   mean_run(x1, k = k, idx = 1:15))
})

test_that("mean_run with idx", {
  expect_identical(
    mean_run(x2, k = 4, idx = idx),
    sapply(window_run(x2, k = 4, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )

  expect_identical(
    mean_run(x2, k = k, idx = idx),
    sapply(window_run(x2, k = k, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )

  expect_equal(
    mean_run(x2, k = 4, lag = 3, idx = idx),
    sapply(window_run(x2, k = 4, lag = 3, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )

  expect_equal(
    mean_run(x2, k = k, lag = 3, idx = idx),
    sapply(window_run(x2, k = k, lag = 3, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )

  expect_equal(
    mean_run(x2, k = 5, lag = lag, idx = idx),
    sapply(window_run(x2, k = 5, lag = lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )

  expect_equal(
    mean_run(x2, k = k, lag = lag, idx = idx),
    sapply(window_run(x2, k = k, lag = lag, idx = idx), function(x) {
      if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
    })
  )
})


