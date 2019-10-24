context("Test Runner")
x1 <- rnorm(100)
x2 <- sample(c(rep(NA, 5), rnorm(15)), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
find_idx  <- function(x, i, lag = 0, k = 1) seq_along(x) %in% seq(i - lag - k + 1, i - lag)
find_idx2 <- function(x, i, lag = 0, k = i) {
  if ((i - lag - k + 1) < 1 || (i - lag) > length(x)) {
    return(x[0])
  } else {
    return(seq_along(x) %in% seq(i - lag - k + 1, i - lag))
  }
}

test_that("constant window", {
  expect_identical(
    runner(x1, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 100)])))

  expect_identical(
    runner(x2, f = mean),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 100)])))


  expect_identical(
    runner(x2, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 100)], na.rm = FALSE)))

  expect_identical(
    runner(x2, k = 5, f = function(x) mean(x, na.rm = TRUE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 5)], na.rm = TRUE)))

  expect_identical(
    runner(x2, k = 5, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 5)], na.rm = FALSE)))

  expect_identical(
    runner(x2, k = 1, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 1)], na.rm = FALSE)))

  expect_identical(
    runner(x2, k = 101, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 101)], na.rm = FALSE)))

  expect_identical(
    runner(x2, k = 0, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = 101)], na.rm = FALSE)))

})

test_that("varying window", {
  expect_identical(
    runner(x1, k = k, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = k[i])])))


  expect_identical(
    runner(x2, k = k, f = mean),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = k[i])])))

  expect_identical(
    runner(x1, k = k, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = k[i])], na.rm = FALSE)))


  expect_identical(
    runner(x2, k = k, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, k = k[i])], na.rm = FALSE)))
})

test_that("varying window na_pad", {
  expect_equal(
    runner(x2, k = k, f = mean, na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx2(x2, i = i, k = k[i])])))

  expect_equal(
    runner(x2, k = k, f = mean, na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx2(x2, i = i, k = k[i])])))

  expect_equal(
    runner(x2, k = k, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx2(x2, i = i, k = k[i])], na.rm = FALSE)))
})

test_that("lagged window", {
  expect_equal(
    runner(x1, k = 5, lag = 3, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = 3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 5, lag = -3, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = -3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = k, lag = 3, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = k[i], lag = 3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = k, lag = -3, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = k[i], lag = -3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 5, lag = lag, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = lag[i])], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 101, lag = lag, f = function(x) mean(x, na.rm = FALSE)),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101, lag = lag[i])], na.rm = FALSE)))

})

test_that("lagged window na_pad", {

  expect_equal(
    runner(x2, lag = 3, f = function(x) mean(x, na.rm = TRUE), na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, lag = 3, k = i)], na.rm = TRUE)))

  expect_equal(
    runner(x2, lag = 15, f = function(x) mean(x, na.rm = TRUE), na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, lag = 15, k = i)], na.rm = TRUE)))

  expect_equal(
    runner(x2, lag = 101, f = function(x) mean(x, na.rm = TRUE), na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, lag = 101, k = i)], na.rm = TRUE)))

  expect_equal(
    runner(x2, lag = -3, f = function(x) mean(x, na.rm = TRUE), na_pad = TRUE),
    sapply(seq_along(x2), function(i) mean(x2[find_idx(x2, i = i, lag = -3, k = i + 3)], na.rm = TRUE)))



  expect_equal(
    runner(x1, k = 5, lag = 3, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = 5, lag = 3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 5, lag = -3, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = 5, lag = -3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = k, lag = 3, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = k[i], lag = 3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = k, lag = -3, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = k[i], lag = -3)], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 5, lag = lag, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = 5, lag = lag[i])], na.rm = FALSE)))

  expect_equal(
    runner(x1, k = 101, lag = lag, f = function(x) mean(x, na.rm = FALSE), na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx2(x1, i = i, k = 101, lag = lag[i])], na.rm = FALSE)))
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
    expect_silent(runner(as.integer(1:100), k = 5, f = length))
    expect_silent(runner(as.integer(1:100), k = k, f = length))
    expect_silent(runner(as.integer(1:100), k = k, idx, f = length))

    expect_silent(runner(sample(letters, 100, replace = TRUE), k = 5, f = length))
    expect_silent(runner(sample(letters, 100, replace = TRUE), k = k, f = length))
    expect_silent(runner(sample(letters, 100, replace = TRUE), k = k, idx, f = length))

    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = 5, f = length))
    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = k, f = length))
    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = k, idx, f = length))

    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = 5, f = length))
    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = k, f = length))
    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = k, idx, f = length))
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
