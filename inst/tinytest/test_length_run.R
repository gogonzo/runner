k  <- sample(1:5, 10, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3), 10, replace = TRUE))

# length_run constant k------
x1 <- rep(NA, 10)
x2 <- rep(NA, 10)

for (i in 1:10) {
  for (j in i:1) {
    if (idx[j] <= (idx[i] - 3)) {
      x1[i] <- i - j
      break
    }
  }
}

for (i in 1:10) {
  for (j in i:1) {
    if (idx[j] <= (idx[i] - k[i])) {
      x2[i] <- i - j
      break
    }
  }
}


tinytest::expect_identical(length_run(k = 3, idx = idx), x1)
tinytest::expect_identical(length_run(k = k, idx = idx), x2)

# Errors" ------
tinytest::expect_error(length_run(idx = integer(0)), "idx should be of length > 0")

tinytest::expect_error(
  length_run(k = 1:9, idx = idx),
  "length of k and length of idx differs"
)
tinytest::expect_error(
  length_run(lag = 1:9, idx = idx),
  "length of lag and length of idx differs"
)

tinytest::expect_error(
  length_run(k = c(NA, 1:9), idx = idx),
  "NA values in k vector"
)
tinytest::expect_error(
  length_run(lag = c(NA, 1:9), idx = idx),
  "NA values in lag vector"
)
tinytest::expect_error(
  length_run(lag = 1:10, idx = c(NA, 1:9)),
  "NA values in idx vector"
)
