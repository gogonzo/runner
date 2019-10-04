context("Test Runner")
x1 <- rnorm(15)
x2 <- sample(c(rep(NA,5), rnorm(15)), 15, replace = TRUE)
k <- sample(1:15, 15, replace = TRUE)
idx <- cumsum(sample(c(1,2,3,4), 15, replace=T))


test_that("constant window", {
  expect_equal(
    mean_run(x1, k = 5, na_rm = FALSE),
    runner(x1, k = 5, f = mean)
  )

  expect_equal(
    mean_run(x2, k = 5, na_rm = FALSE),
    runner(x2, k = 5, f = mean)
  )

  expect_equal(
    mean_run(x2, k = 5, na_rm = TRUE),
    runner(x2, k = 5, f = function(x) mean(x, na.rm = TRUE))
  )
})

test_that("varying window", {
  expect_equal(
    mean_run(x1, k = k, na_rm = FALSE),
    runner(x1, k = k, f = mean)
  )

  expect_equal(
    mean_run(x2, k = k, na_rm = FALSE),
    runner(x2, k = k, f = mean)
  )

  expect_equal(
    mean_run(x2, k = k, na_rm = TRUE),
    runner(x2, k = k, f = function(x) mean(x, na.rm = TRUE))
  )
})


test_that("date window", {
  expect_equal(
    mean_run(x1, k = k, na_rm = FALSE, idx = idx),
    runner(x1, k = k, idx = idx, f = mean)
  )

  expect_equal(
    mean_run(x2, k = k, na_rm = FALSE, idx = idx),
    runner(x2, k = k, idx = idx, f = mean)
  )

  expect_equal(
    mean_run(x2, k = k, na_rm = TRUE, idx = idx),
    runner(x2, k = k, idx = idx, f = function(x) mean(x, na.rm = TRUE))
  )
})

test_that("Lagged date window", {
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 10, 0.5)
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


})

test_that("Function applied on other types", {
    expect_silent(runner(as.integer(1:15), k = 5, f = length))
    expect_silent(runner(as.integer(1:15), k = k, f = length))
    expect_silent(runner(as.integer(1:15), k = k, idx, f = length))

    expect_silent(runner(letters[1:15], k = 5, f = length))
    expect_silent(runner(letters[1:15], k = k, f = length))
    expect_silent(runner(letters[1:15], k = k, idx, f = length))

    expect_silent(runner(as.factor(letters[1:15]), k = 5, f = length))
    expect_silent(runner(as.factor(letters[1:15]), k = k, f = length))
    expect_silent(runner(as.factor(letters[1:15]), k = k, idx, f = length))

    expect_silent(runner(as.Date(1:15, origin = "1970-01-01"), k = 5, f = length))
    expect_silent(runner(as.Date(1:15, origin = "1970-01-01"), k = k, f = length))
    expect_silent(runner(as.Date(1:15, origin = "1970-01-01"), k = k, idx, f = length))
})
