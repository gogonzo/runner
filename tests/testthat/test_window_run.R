context("Running window")
set.seed(11)
x1 <- 1:30
x2 <- letters[1:30]
k <- sample(1:30, 30, replace = TRUE)
find_idx <- function(x, i, lag = 0, k = 1) seq_along(x) %in% seq(i - lag - k + 1, i - lag)

# add tests with omit_incomplete

test_that("window_run k = constant",{
  for(i in 1:30)
    expect_equal(
      window_run(x1)[i][[1]],
      x1[seq_len(i)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2)[i][[1]],
      x2[seq_len(i)]
    )

  for(i in 1:30)
    expect_equal(
     window_run(x1, k = 2)[i][[1]],
     x1[find_idx(x1, i = i, k = 2)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 2)[i][[1]],
      x2[find_idx(x2, i = i, k = 2)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(as.character(x2), k = 2)[i][[1]],
      as.character(x2[find_idx(x2, i = i, k = 2)])
    )

  for(i in 1:30)
    expect_equal(
      window_run(as.numeric(x1), k = 2)[i][[1]],
      as.numeric(x1[find_idx(x1, i = i, k = 2)])
    )

  for(i in 1:30)
    expect_equal(
      window_run(as.character(x2), k = 50)[i][[1]],
      as.character(x2[find_idx(x2, i = i, k = 50)])
    )

  for(i in 1:30)
    expect_equal(
      window_run(as.character(x2), k = 0)[i][[1]],
      as.character(x2[find_idx(x2, i = i, k = 50)])
    )

})

test_that("window_run with k varying", {
  for(i in 1:30)
    expect_equal(
      window_run(x2, k = k)[i][[1]],
      x2[find_idx(x2, i = i, k = k[i])]
    )
})

test_that("window_run with lag", {
  lag <- sample(-15:15, 30, replace = TRUE)

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 5, lag = 3)[i][[1]],
      x2[find_idx(x2, i = i, k = 5, lag = 3)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 20, lag = 3)[i][[1]],
      x2[find_idx(x2, i = i, k = 20, lag = 3)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 50, lag = 20)[i][[1]],
      x2[find_idx(x2, i = i, k = 50, lag = 20)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = k, lag = 3)[i][[1]],
      x2[find_idx(x2, i = i, k = k[i], lag = 3)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 5, lag = -3)[i][[1]],
      x2[find_idx(x2, i = i, k = 5, lag = -3)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 5, lag = 21)[i][[1]],
      x2[find_idx(x2, i = i, k = 5, lag = 21)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = 5, lag = -21)[i][[1]],
      x2[find_idx(x2, i = i, k = 5, lag = -21)]
    )

  for(i in 1:30)
    expect_equal(
      window_run(x2, k = k, lag = lag)[i][[1]],
      x2[find_idx(x2, i = i, k = k[i], lag = lag[i])]
    )
})

test_that("window_run with idx same as window_run with windows",{
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 30, 0.5)
  lag <- sample(-25:25, 100, replace = TRUE)

  expect_identical(window_run(x, k = 3) ,
                   window_run(x, k = 3, idx = 1:100))
  expect_identical(window_run(x, k = k) ,
                   window_run(x, k = k, idx = 1:100))

  expect_identical(window_run(x, k = k, lag = 5),
                   window_run(x, k = k, lag = 5, idx = 1:100))

  expect_identical(window_run(x, k = k, lag = lag),
                   window_run(x, k = k, lag = lag, idx = 1:100))
})

test_that("Lagged date window", {
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 10, 0.5)
  lag <- sample(-25:25, 100, replace = TRUE)
  idx <- cumsum(sample(c(1,2,3,4), 100, replace = TRUE))

  out <- window_run(x, k = 5, lag = 3, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] - 3 - 5  + 1
    upper <- idx[i] - 3
    x[idx %in% seq(lower, upper)]
  })
  expect_equal(out, test)

  out <- window_run(x, k = k, lag = 3, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] - 3 - k[i]  + 1
    upper <- idx[i] - 3
    x[idx %in% seq(lower, upper)]
  })

  expect_equal(out, test)

  out <- window_run(x, k = k, lag = lag, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] - lag[i] - k[i]  + 1
    upper <- idx[i] - lag[i]
    x[idx %in% seq(lower, upper)]
  })

  expect_equal(out, test)
})

test_that("Negative lagged date window", {
  x <- sample(c(rep(NA, 20), runif(100)), 100)
  k <- qbinom(runif(100, 0.2, 0.8), 10, 0.5)
  lag <- sample(-25:25, 100, replace = TRUE)
  idx <- cumsum(sample(c(1,2,3,4), 100, replace = TRUE))

  out <- window_run(x, k = 5, lag = -3, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] + 3 - 5  + 1
    upper <- idx[i] + 3
    x[idx %in% seq(lower, upper)]
  })
  expect_equal(out, test)

  out <- window_run(x, k = k, lag = -3, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] + 3 - k[i]  + 1
    upper <- idx[i] + 3
    x[idx %in% seq(lower, upper)]
  })

  expect_equal(out, test)

  out <- window_run(x, k = k, lag = lag, idx = idx)
  test <- lapply(seq_along(x), function(i) {
    lower <- idx[i] - lag[i] - k[i]  + 1
    upper <- idx[i] - lag[i]
    x[idx %in% seq(lower, upper)]
  })

  expect_equal(out, test)
})

test_that("window_run with idx",{
  x11 <- list()
  x22 <- list()
  idx <- cumsum(sample(c(1, 2, 3, 4), 30, replace = TRUE))

  for(i in 1:30)
    for(j in i:1)
      if(idx[j] >= (idx[i] - 2)){
        x11[[i]] <- x1[j:i]
      } else {
        break;
      }

  for(i in 1:30)
    for(j in i:1)
      if(idx[j] >= (idx[i] - (k[i] - 1))) {
        x22[[i]] <- x1[j:i]
      } else {
        break;
      }

  expect_identical(window_run(x1, k = 3, idx = idx), x11)
  expect_identical(window_run(x1, k = k, idx = idx), x22)
})

test_that("Test non-numeric arguments", {
  expect_silent(window_run(as.integer(1:10), k = 5))
  expect_silent(window_run(letters[1:10], k = 5))
  expect_silent(window_run(as.factor(letters[1:10]), k = 5))
  expect_silent(window_run(seq(Sys.Date(), Sys.Date() + 9, by = "1 day"), k = 5))
  expect_silent(window_run(sample(c(TRUE, FALSE), 10, replace = TRUE), k = 5))
  expect_silent(window_run(as.complex(1:10), k = 5))

})

test_that("Errors", {
  expect_error(window_run(list(1:10), k = 5), "Invalid data type")

  expect_error(window_run(1:10, k = (1:9)), "length of k and length of x differs")
  expect_error(window_run(1:10, k = c(NA, 1:9)), "Function doesn't accept NA values in k vector")

  expect_error(window_run(1:10, lag = (1:9)), "length of lag and length of x differs")
  expect_error(window_run(1:10, lag = c(NA, 1:9)), "Function doesn't accept NA values in lag vector")

  expect_error(window_run(1:10, idx = (1:9)), "length of idx and length of x differs")
  expect_error(window_run(1:10, idx = c(NA, 1:9)), "Function doesn't accept NA values in idx vector")
})
