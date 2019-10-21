context("Test Runner")
x1 <- rnorm(30)
x2 <- sample(c(rep(NA, 5), rnorm(15)), 30, replace = TRUE)
k <- sample(1:15, 30, replace = TRUE)
lag <- sample(-5:5, 30, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 30, replace = TRUE))

test_that("constant window", {
  expect_equal(
    runner(x = x1, f = mean),
    sapply(window_run(x1), mean)
  )

  expect_equal(
    runner(x1, k = 5, f = mean),
    sapply(window_run(x1, k = 5), mean)
  )

  expect_equal(
    runner(x2, k = 5, f = function(x) mean(x, na.rm = FALSE)),
    sapply(window_run(x2, k = 5), mean, na.rm = FALSE)
  )

  expect_equal(
    runner(x2, k = 5, f = function(x) mean(x, na.rm = TRUE)),
    sapply(window_run(x2, k = 5), mean, na.rm = TRUE)
  )
})

test_that("varying window", {
  expect_equal(
    runner(x1, k = k, f = mean),
    sapply(window_run(x1, k = k), mean)
  )

  expect_equal(
    runner(x2, k = k, f = mean),
    sapply(window_run(x2, k = k), mean)
  )

  expect_equal(
    runner(x2, k = k, f = function(x) mean(x, na.rm = TRUE)),
    sapply(window_run(x2, k = k), mean, na.rm = TRUE)
  )
})

test_that("lagged window", {
  out <- runner(x1, lag = 3, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- 0
    upper <- i - 3
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper
    idx <- seq(lower, upper)
    mean(x1[idx], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, lag = lag, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- 0
    upper <- i - lag[i]
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper

    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, k = 5, lag = 3, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- i - 3 - 5 + 1
    upper <- i - 3
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper
    idx <- seq(lower, upper)
    mean(x1[idx], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, k = 5, lag = lag, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- i - lag[i] - 5 + 1
    upper <- i - lag[i]
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper

    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)
})

test_that("negative lagged window", {

  out <- runner(x1, lag = -3, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- 0
    upper <- i + 3
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper
    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, lag = lag, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- 0
    upper <- i - lag[i]
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper

    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, k = 5, lag = -3, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- i + 3 - 5 + 1
    upper <- i + 3
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper

    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_identical(test, out)

  out <- runner(x1, k = 5, lag = lag, f = function(x) mean(x, na.rm = FALSE))
  test <- vapply(seq_along(x1), function(i) {
    lower <- i - lag[i] - 5 + 1
    upper <- i - lag[i]
    lower <- if (lower < 1) 1 else if (lower > length(x1)) return(NA_real_) else lower
    upper <- if (upper < 1) return(NA_real_) else if (upper > length(x1)) length(x1) else upper

    mean(x1[seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_equal(test, out)
})

test_that("date window", {
  expect_equal(
    runner(x1, k = k, idx = idx, f = mean),
    sapply(window_run(x1, k = k, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = k, idx = idx, f = mean),
    sapply(window_run(x2, k = k, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = k, idx = idx, f = function(x) mean(x, na.rm = TRUE)),
    sapply(window_run(x2, k = k, idx = idx), mean, na.rm = TRUE)
  )
})

test_that("Lagged date window", {
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 10, 0.5)
  lag <- sample(-15:15, 100, replace = TRUE)
  idx <- cumsum(sample(c(1,2,3,4), 100, replace = TRUE))

  out <- runner(x, k = 5, lag = 3, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  test <- vapply(seq_along(x), function(i) {
    lower <- idx[i] - 3 - 5  + 1
    upper <- idx[i] - 3
    mean(x[idx %in% seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))

  expect_equal(out, test)


  out <- runner(x, k = k, lag = 3, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  test <- vapply(seq_along(x), function(i) {
    lower <- idx[i] - 3 - k[i]  + 1
    upper <- idx[i] - 3
    mean(x[idx %in% seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_equal(out, test)

  out <- runner(x, k = k, lag = lag, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  test <- vapply(seq_along(x), function(i) {
    lower <- idx[i] - lag[i] - k[i]  + 1
    upper <- idx[i] - lag[i]
    mean(x[idx %in% seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))

  expect_equal(out, test)
})

test_that("Negative lag date window", {
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 10, 0.5)
  lag <- sample(-15:15, 100, replace = TRUE)
  idx <- cumsum(sample(c(1,2,3,4), 100, replace = TRUE))

  out <- runner(x, k = 5, lag = -3, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  test <- vapply(seq_along(x), function(i) {
    lower <- idx[i] + 3 - 5  + 1
    upper <- idx[i] + 3
    mean(x[idx %in% seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_equal(out, test)

  out <- runner(x, k = k, lag = lag, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  test <- vapply(seq_along(x), function(i) {
    lower <- idx[i] - lag[i] - k[i]  + 1
    upper <- idx[i] - lag[i]
    mean(x[idx %in% seq(lower, upper)], na.rm = TRUE)
  }, numeric(1))
  expect_equal(out, test)
})

test_that("Function applied on other types", {
    expect_silent(runner(as.integer(1:30), k = 5, f = length))
    expect_silent(runner(as.integer(1:30), k = k, f = length))
    expect_silent(runner(as.integer(1:30), k = k, idx, f = length))

    expect_silent(runner(letters[1:30], k = 5, f = length))
    expect_silent(runner(letters[1:30], k = k, f = length))
    expect_silent(runner(letters[1:30], k = k, idx, f = length))

    expect_silent(runner(as.factor(letters[1:30]), k = 5, f = length))
    expect_silent(runner(as.factor(letters[1:30]), k = k, f = length))
    expect_silent(runner(as.factor(letters[1:30]), k = k, idx, f = length))

    expect_silent(runner(as.Date(1:30, origin = "1970-01-01"), k = 5, f = length))
    expect_silent(runner(as.Date(1:30, origin = "1970-01-01"), k = k, f = length))
    expect_silent(runner(as.Date(1:30, origin = "1970-01-01"), k = k, idx, f = length))
})

test_that("Errors", {
  expect_error(runner(x = letters[1:5]))
  expect_error(runner(x = letters[1:5], f = ""))

  expect_error(runner(list(1:10), k = 5, f = mean), "Invalid data type")

  expect_error(runner(1:10, k = (1:9), f = mean), "length of k and length of x differs")
  expect_error(runner(1:10, k = c(NA, 1:9), f = mean), "Function doesn't accept NA values in k vector")

  expect_error(runner(1:10, lag = (1:9), f = mean), "length of lag and length of x differs")
  expect_error(runner(1:10, lag = c(NA, 1:9), f = mean), "Function doesn't accept NA values in lag vector")

  expect_error(runner(1:10, idx = (1:9), f = mean), "length of idx and length of x differs")
  expect_error(runner(1:10, idx = c(NA, 1:9), f = mean), "Function doesn't accept NA values in idx vector")
})
