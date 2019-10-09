context("Running lag")
x <- 1:30
k  <- sample(1:10, 30, replace = TRUE)
idx <- cumsum(sample(seq_len(5), 30, replace = TRUE))

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
  comp <- ifelse((x - k) <= 0, NA, x - k)
  expect_identical(lag_run(x, k = k), comp)
})

test_that("lag_run moving idx window (nearest end)", {
  x11 <- rep(NA, length(x))
  x12 <- rep(NA, length(x))

  for(i in 1:30) {
    for(j in i:1) {
      if(idx[j] <= (idx[i] - 3)) {
        x11[i] <- x[j]
        break
      }
    }
  }

  for(i in 1:30) {
    for(j in i:1) {
      if(idx[j] <= (idx[i] - k[i])) {
        x12[i] <- x[j]
        break
      }
    }
  }

  expect_identical(lag_run(x, k = 3, idx = idx, nearest = TRUE), x11)
  expect_identical(lag_run(x, k = k, idx = idx, nearest = TRUE), x12)
})

test_that("lag_run moving idx window (exact end)", {
  x21 <- rep(NA, length(x))
  x22 <- rep(NA, length(x))
  for(i in 1:30) {
    for(j in i:1) {
      if(idx[j] == (idx[i] - 3)) {
        x21[i] <- x[j]
        break
      }
    }
  }



  for(i in 1:30) {
    for(j in i:1) {
      if(idx[j] == (idx[i] - k[i])) {
        x22[i] <- x[j]
        break
      }
    }
  }

  expect_identical(lag_run(x, k = 3, idx = idx, nearest = FALSE), x21)
  expect_identical(lag_run(x, k = k, idx = idx, nearest = FALSE), x22)

})

test_that("Errors", {
  expect_error(lag_run(x, k = (1:9)), "length of k and length of x differs")
  expect_error(min_run(x, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(lag_run(x, idx = (1:9)), "length of idx and length of x differs")
  expect_error(lag_run(x, idx = c(NA, 1:29)), "Function doesn't accept NA values in idx vector")
})

