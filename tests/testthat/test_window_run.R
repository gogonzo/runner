context("Running window")
set.seed(11)
x1 <- 1:100
x2 <- as.character(1:100)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
find_idx  <- function(x, i, lag = 0, k = 1) seq_along(x) %in% seq(i - lag - k + 1, i - lag)
find_idx2 <- function(x, i, lag = 0, k = 1) {
  if ((i - lag - k + 1) < 1 || (i - lag) > length(x)) {
    return(x[0])
  } else {
    return(seq_along(x) %in% seq(i - lag - k + 1, i - lag))
  }
}

# add tests with omit_incomplete

test_that("window_run k = constant",{
  expect_identical(
    window_run(x1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 100)])
  )

  expect_identical(
    window_run(x2),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 100)])
  )

  expect_identical(
    window_run(x2, k = 2),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 2)])
  )

  expect_identical(
    window_run(x2, k = 2),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 2)])
  )

})

test_that("window_run window maximum", {

  expect_identical(
    window_run(x2, k = 10000),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 10000)])
  )

  expect_identical(
    window_run(x2, k = 0),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 10000)])
  )

})

test_that("window_run with k varying", {
  expect_identical(
    window_run(x2, k = k),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = k[i])])
  )

})

test_that("window_run with lag", {

  expect_identical(
    window_run(x2, k = 5, lag = 3),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 5, lag = 3)])
  )

  expect_identical(
    window_run(x2, k = 120, lag = 3),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 120, lag = 3)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = 30),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = 30)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -30),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = -30)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -1),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = -1)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -1),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = -1)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -100),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = -100)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -100),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = -100)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = 101),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = 101)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = 101),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = 101)])
  )

  expect_identical(
    window_run(x2, k = k, lag = 3),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = k[i], lag = 3)])
  )

  expect_identical(
    window_run(x2, k = k, lag = lag),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = k[i], lag = lag[i])])
  )

  expect_identical(
    window_run(x2, k = 3, lag = lag),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 3, lag = lag[i])])
  )


  expect_identical(
    window_run(x2, k = 300, lag = lag),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 300, lag = lag[i])])
  )

  expect_identical(
    window_run(x2, k = 1, lag = lag),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 1, lag = lag[i])])
  )

  expect_identical(
    window_run(x2, k = 0, lag = lag),
    lapply(seq_along(x2), function(i) x2[find_idx(x2, i = i, k = 5000, lag = lag[i])])
  )

})

test_that("window_run window omit_incomplete", {

  expect_identical(
    window_run(x2, k = 5, lag = 3, omit_incomplete = TRUE)[4],
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 5, lag = 3)])[4]
  )

  expect_identical(
    window_run(x2, k = 120, lag = 3, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 120, lag = 3)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = 30, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 1, lag = 30)])
  )

  expect_identical(
    window_run(x2, k = 1, lag = -30, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 1, lag = -30)])
  )

  expect_identical(
    window_run(x2, k = k, lag = 3, omit_incomplete = TRUE)[16],
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = k[i], lag = 3)])[16]
  )

  expect_identical(
    window_run(x2, k = k, lag = lag, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = k[i], lag = lag[i])])
  )

  expect_identical(
    window_run(x2, k = 3, lag = lag, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 3, lag = lag[i])])
  )


  expect_identical(
    window_run(x2, k = 300, lag = lag, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 300, lag = lag[i])])
  )

  expect_identical(
    window_run(x2, k = 1, lag = lag, omit_incomplete = TRUE),
    lapply(seq_along(x2), function(i) x2[find_idx2(x2, i = i, k = 1, lag = lag[i])])
  )

})

test_that("window_run with idx same as window_run with windows",{
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
  out <- window_run(x2, k = 5, lag = 3, idx = idx)
  test <- lapply(seq_along(x2), function(i) {
    lower <- idx[i] - 3 - 5  + 1
    upper <- idx[i] - 3
    x2[idx %in% seq(lower, upper)]
  })
  expect_equal(out, test)

  out <- window_run(x2, k = k, lag = 3, idx = idx)
  test <- lapply(seq_along(x2), function(i) {
    lower <- idx[i] - 3 - k[i]  + 1
    upper <- idx[i] - 3
    x2[idx %in% seq(lower, upper)]
  })

  expect_equal(out, test)

  out <- window_run(x2, k = k, lag = lag, idx = idx)
  test <- lapply(seq_along(x2), function(i) {
    lower <- idx[i] - lag[i] - k[i]  + 1
    upper <- idx[i] - lag[i]
    x2[idx %in% seq(lower, upper)]
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
  idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))

  for(i in 1:100)
    for(j in i:1)
      if(idx[j] >= (idx[i] - 2)){
        x11[[i]] <- x1[j:i]
      } else {
        break;
      }

  for(i in 1:100)
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
