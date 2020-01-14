context("Running which")
set.seed(11)
x1 <- sample(c(T, F), 100, replace = TRUE)
x2 <- sample(c(NA, T, F), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
which2 <- function(x, arg_which = "last", na_rm = TRUE) {
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
  return(x)
}

test_that("       |--------]------->", {
  expect_identical(
    which_run(x2),
    as.integer(runner(x2, f = which2))
  )

  expect_identical(
    which_run(x2, na_pad = TRUE),
    as.integer(runner(x2, f = which2, na_pad = TRUE))
  )


  expect_identical(
    which_run(x2, which = "first"),
    as.integer(runner(x2, function(x) which2(x, arg_which = "first")))
  )

  expect_identical(
    which_run(x2, which = "first", na_pad = TRUE),
    as.integer(runner(x2, function(x) which2(x, arg_which = "first"), na_pad = TRUE))
  )

  expect_identical(
    which_run(x1, na_rm = FALSE),
    as.integer(runner(x1, function(x) which2(x, na_rm = FALSE)))
  )


  expect_identical(
    which_run(x1, which = "first", na_rm = FALSE),
    as.integer(runner(x1, function(x) which2(x, arg_which = "first", na_rm = FALSE)))
  )
})

test_that("   [...|----]---+------->", {
  expect_equal(
    which_run(x2, lag = 3),
    as.integer(runner(x2, lag = 3, f = which2)))

  expect_equal(
    which_run(x2, lag = 3, na_pad = TRUE),
    as.integer(runner(x2, lag = 3, f = which2, na_pad = TRUE)))
})

test_that("       |--------+---]--->", {
  expect_equal(
    which_run(x2, lag = -3),
    as.integer(runner(x2, lag = -3, f = which2)))

  expect_equal(
    which_run(x2, lag = -3, na_pad = TRUE),
    as.integer(runner(x2, lag = -3, f = which2, na_pad = TRUE)))
})

test_that("  [...]|--------+------->", {
  expect_equal(
    which_run(x2, lag = 100),
    as.integer(runner(x2, lag = 100, f = which2)))

  expect_equal(
    which_run(x2, lag = 100, na_pad = TRUE),
    as.integer(runner(x2, lag = 100, f = which2, na_pad = TRUE)))


  expect_equal(
    which_run(x2, lag = -100),
    as.integer(runner(x2, lag = -100, f = which2)))

  expect_equal(
    which_run(x2, lag = -100, na_pad = TRUE),
    as.integer(runner(x2, lag = -100, f = which2, na_pad = TRUE)))
})

test_that("       |----[...]------->", {
  out <- which_run(x2, k = 10)
  test <- runner(x2, k = 10, f = which2)
  test[10:100] <- test[10:100] + 10:100L - 10L
  expect_equal(out, test)

  out <- which_run(x2, k = 10, na_pad = TRUE)
  test <- runner(x2, k = 10, f = which2, na_pad = TRUE)
  test[10:100] <- test[10:100] + 10:100L - 10L
  expect_equal(out, test)


  out <- which_run(x2, k = 10, which = "first")
  test <- runner(x2, k = 10, f = function(x) which2(x = x, arg_which = "first"))
  test[10:100] <- test[10:100] + 10:100L - 10L
  expect_equal(out, test)

  out <- which_run(x2, k = 10, na_pad = TRUE, which = "first")
  test <- runner(x2, k = 10, f = function(x) which2(x = x, arg_which = "first"), na_pad = TRUE)
  test[10:100] <- test[10:100] + 10:100L - 10L
  expect_equal(out, test)
})

test_that("       [...|--------+-------[...]", {
  out <- which_run(x2, k = 1)
  test <- runner(x2, k = 1, f = which2)
  test[1:100] <- test[1:100] + 1:100L - 1L
  expect_equal(out, test)

  out <- which_run(x2, k = 1, na_pad = TRUE)
  test <- runner(x2, k = 1, f = which2, na_pad = TRUE)
  test[1:100] <- test[1:100] + 1:100L - 1L
  expect_equal(out, test)
})

test_that("       [...|----]---+------->", {
  out <- which_run(x2, k = 5, lag = 3)
  test <- runner(x2, k = 5, lag = 3, f = which2)
  test[(5 + 3):100] <- test[(5 + 3):100] + (5 + 3):100L - (5L + 3)
  expect_equal(out, test)

  out <- which_run(x2, k = 5, lag = 3, na_pad = TRUE)
  test <- runner(x2, k = 5, lag = 3, f = which2, na_pad = TRUE)
  test[(5 + 3):100] <- test[(5 + 3):100] + (5 + 3):100L - (5L + 3)
  expect_equal(out, test)

  out <- which_run(x2, k = 5, lag = 3, na_pad = TRUE, na_rm = FALSE)
  test <- runner(x2, k = 5, lag = 3, f = function(x) which2(x, na_rm = FALSE), na_pad = TRUE)
  test[(5 + 3):100] <- test[(5 + 3):100] + (5 + 3):100L - (5L + 3)
  expect_equal(out, test)
})

test_that("       |-----[--+---]--->", {
  out <- which_run(x2, k = 5, lag = -3)
  test <- runner(x2, k = 5, lag = -3, f = which2)
  test[(5 - 3):100] <- test[(5 - 3):100] + (5 - 3):100L - (5L - 3)
  expect_equal(out, test)

  out <- which_run(x2, k = 5, lag = -3, na_pad = TRUE)
  test <- runner(x2, k = 5, lag = -3, f = which2, na_pad = TRUE)
  test[(5 - 3):100] <- test[(5 - 3):100] + (5 - 3):100L - (5L - 3)
  expect_equal(out, test)

  out <- which_run(x2, k = 5, lag = -3, na_pad = TRUE, na_rm = FALSE)
  test <- runner(x2, k = 5, lag = -3, f = function(x) which2(x, na_rm = FALSE), na_pad = TRUE)
  test[(5 - 3):100] <- test[(5 - 3):100] + (5 - 3):100L - (5L - 3)
  expect_equal(out, test)
})

test_that("idx", {
  expect_equal(
    which_run(x2, lag = 3, idx = 1:100),
    which_run(x2, lag = 3))

  expect_equal(
    which_run(x2, lag = 3, idx = 1:100, na_pad = TRUE),
    which_run(x2, lag = 3, na_pad = TRUE))

  expect_equal(
    which_run(x2, lag = -3, idx = 1:100),
    which_run(x2, lag = -3))

  expect_equal(
    which_run(x2, lag = -3, idx = 1:100, na_pad = TRUE),
    which_run(x2, lag = -3, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 5, lag = 3, idx = 1:100),
    which_run(x2, k = 5, lag = 3))

  expect_equal(
    which_run(x2, k = 5, lag = 3, idx = 1:100, na_pad = TRUE),
    which_run(x2, k = 5, lag = 3, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 5, lag = -3, idx = 1:100),
    which_run(x2, k = 5, lag = -3))

  expect_equal(
    which_run(x2, k = 5, lag = -3, idx = 1:100, na_pad = TRUE),
    which_run(x2, k = 5, lag = -3, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 5, lag = -3, idx = 1:100),
    which_run(x2, k = 5, lag = -3))

  expect_equal(
    which_run(x2, k = 5, lag = -3, idx = 1:100, na_pad = TRUE),
    which_run(x2, k = 5, lag = -3, na_pad = TRUE))


  expect_equal(
    which_run(x2, k = 5, idx = 1:100),
    which_run(x2, k = 5))

  expect_equal(
    which_run(x2, k = 5, idx = 1:100, na_pad = TRUE),
    which_run(x2, k = 5, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100),
    which_run(x2, k = k, lag = lag))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100, na_pad = TRUE),
    which_run(x2, k = k, lag = lag, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100, na_rm = FALSE),
    which_run(x2, k = k, lag = lag, na_rm = FALSE))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100, na_pad = TRUE, na_rm = FALSE),
    which_run(x2, k = k, lag = lag, na_pad = TRUE, na_rm = FALSE))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100, na_rm = FALSE),
    which_run(x2, k = k, lag = lag, na_rm = FALSE))

  expect_equal(
    which_run(x2, k = k, lag = lag, idx = 1:100, na_pad = TRUE, na_rm = FALSE),
    which_run(x2, k = k, lag = lag, na_pad = TRUE, na_rm = FALSE))


})

test_that("Errors", {
  expect_error(which_run(x1, which = "any"), "which value should be either")
  expect_error(which_run(x1, k = (1:999)), "length of k and length of x differs")
  expect_error(which_run(x1, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(which_run(x1, lag = (1:99)), "length of lag and length of x differs")
  expect_error(which_run(x1, lag = c(NA, lag[-1])), "Function doesn't accept NA values in lag vector")

  expect_error(which_run(x1, idx = (1:99)), "length of idx and length of x differs")
  expect_error(which_run(x1, idx = c(NA, 1:99)), "Function doesn't accept NA values in idx vector")
})
