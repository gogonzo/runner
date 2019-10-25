context("Running min")
set.seed(11)
x1 <- sample(c(1, 2, 3), 100, replace = TRUE)
x2 <- sample(c(NA, 1, 2, 3), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
which2 <- function(x, arg_which, na_rm, i, k, lag = 0, which) {
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
  if (!missing(k)) {
    if (!missing(i)) {
      return(as.integer(x + (i - 1) - k - lag + 1))
    } else {
      return(as.integer(x - k - lag + 1))
    }
  } else {
    if (!missing(i)) {
      return(as.integer(x + (i - 1) - lag))
    } else {
      return(as.integer(x - lag))
    }
  }

}

test_that("       |--------]------->", {
  expect_identical(
    which_run(x2),
    runner(x2, f = which2)
  )

  expect_identical(
    which_run(x2, na_pad = TRUE),
    runner(x2, f = which2, na_pad = TRUE)
  )
})

test_that("   [...|----]---+------->", {
  expect_equal(
    which_run(x2, lag = 3),
    runner(x2, lag = 3, f = which2))

  expect_equal(
    which_run(x2, lag = 3, na_pad = TRUE),
    runner(x2, lag = 3, f = which2, na_pad = TRUE))
})

test_that("       |--------+---]--->", {
  expect_equal(
    which_run(x2, lag = -3),
    runner(x2, lag = -3, f = which2))

  expect_equal(
    which_run(x2, lag = -3, na_pad = TRUE),
    runner(x2, lag = -3, f = which2, na_pad = TRUE))
})

test_that("  [...]|--------+------->", {
  expect_equal(
    which_run(x2, lag = 100),
    runner(x2, lag = 100, f = which2))

  expect_equal(
    which_run(x2, lag = 100, na_pad = TRUE),
    runner(x2, lag = 100, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, lag = -100),
    runner(x2, lag = -100, f = which2))

  expect_equal(
    which_run(x2, lag = -100, na_pad = TRUE),
    runner(x2, lag = -100, f = which2, na_pad = TRUE))
})

test_that("       |----[...]------->", {
  expect_equal(
    which_run(x2, k = 3),
    runner(x2, k = 3, f = which2))

  expect_equal(
    which_run(x2, k = 3, na_pad = TRUE),
    runner(x2, k = 3, f = which2, na_pad = TRUE))

})

test_that("       [...|--------+-------[...]", {
  expect_equal(
    which_run(x2, k = 1),
    runner(x2, k = 1, f = which2))

  expect_equal(
    which_run(x2, k = 1, na_pad = TRUE),
    runner(x2, k = 1, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 99),
    runner(x2, k = 99, f = which2))

  expect_equal(
    which_run(x2, k = 99, na_pad = TRUE),
    runner(x2, k = 99, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 100),
    runner(x2, k = 100, f = which2))

  expect_equal(
    which_run(x2, k = 100, na_pad = TRUE),
    runner(x2, k = 100, f = which2, na_pad = TRUE))
})

test_that("       [...|----]---+------->", {
  expect_equal(
    which_run(x2, k = 5, lag = 3),
    runner(x2, k = 5, lag = 3, f = which2))

  expect_equal(
    which_run(x2, k = 5, lag = 3, na_pad = TRUE),
    runner(x2, k = 5, lag = 3, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 5, lag = 3, na_rm = FALSE),
    runner(x2, k = 5, lag = 3, f = which))

  expect_equal(
    which_run(x2, k = 5, lag = 3, na_pad = TRUE, na_rm = FALSE),
    runner(x2, k = 5, lag = 3, f = which, na_pad = TRUE))
})

test_that("       |-----[--+---]--->", {
  expect_equal(
    which_run(x2, k = 5, lag = -3),
    runner(x2, k = 5, lag = -3, f = which2))

  expect_equal(
    which_run(x2, k = 5, lag = -3, na_pad = TRUE),
    runner(x2, k = 5, lag = -3, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 5, lag = -3, na_rm = FALSE),
    runner(x2, k = 5, lag = -3, f = which))

  expect_equal(
    which_run(x2, k = 5, lag = -3, na_pad = TRUE, na_rm = FALSE),
    runner(x2, k = 5, lag = -3, f = which, na_pad = TRUE))
})

test_that("       |--------+-[---]->", {
  expect_equal(
    which_run(x2, k = 5, lag = -7),
    runner(x2, k = 5, lag = -7, f = which2))

  expect_equal(
    which_run(x2, k = 5, lag = -7, na_pad = TRUE),
    runner(x2, k = 5, lag = -7, f = which2, na_pad = TRUE))

})

test_that("       |--------+[]----->", {
  expect_equal(
    which_run(x2, k = 1, lag = -1),
    runner(x2, k = 1, lag = -1, f = which2))

  expect_equal(
    which_run(x2, k = 1, lag = -1, na_pad = TRUE),
    runner(x2, k = 1, lag = -1, f = which2, na_pad = TRUE))
})

test_that("       |------[]+------->", {
  expect_equal(
    which_run(x2, k = 1, lag = 1),
    runner(x2, k = 1, lag = 1, f = which2))

  expect_equal(
    which_run(x2, k = 1, lag = 1, na_pad = TRUE),
    runner(x2, k = 1, lag = 1, f = which2, na_pad = TRUE))
})

test_that("various", {
  expect_equal(
    which_run(x2, k = k, lag = 1),
    runner(x2, k = k, lag = 1, f = which2))

  expect_equal(
    which_run(x2, k = k, lag = 1, na_pad = TRUE),
    runner(x2, k = k, lag = 1, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, k = 3, lag = lag),
    runner(x2, k = 3, lag = lag, f = which2))

  expect_equal(
    which_run(x2, k = 3, lag = lag, na_pad = TRUE),
    runner(x2, k = 3, lag = lag, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = k, lag = lag),
    runner(x2, k = k, lag = lag, f = which2))

  expect_equal(
    which_run(x2, k = k, lag = lag, na_pad = TRUE),
    runner(x2, k = k, lag = lag, f = which2, na_pad = TRUE))

})

test_that("date window", {
  expect_equal(
    which_run(x2, lag = 3, idx = idx, na_pad = FALSE),
    runner(x2, lag = 3, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = 3, idx = idx, na_pad = TRUE),
    runner(x2, lag = 3, idx = idx, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, lag = -3, idx = idx, na_pad = FALSE),
    runner(x2, lag = -3, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = -3, idx = idx, na_pad = TRUE),
    runner(x2, lag = -3, idx = idx, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 3, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, k = 3, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, idx = idx, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, lag = -1, idx = idx, na_pad = FALSE),
    runner(x2, lag = -1, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = -1, idx = idx, na_pad = TRUE),
    runner(x2, lag = -1, idx = idx, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, lag = 100, idx = idx, na_pad = FALSE),
    runner(x2, lag = 100, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = 100, idx = idx, na_pad = TRUE),
    runner(x2, lag = 100, idx = idx, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, lag = -100, idx = idx, na_pad = FALSE),
    runner(x2, lag = -100, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = -100, idx = idx, na_pad = TRUE),
    runner(x2, lag = -100, idx = idx, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, lag = lag, idx = idx, na_pad = FALSE),
    runner(x2, lag = lag, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, lag = lag, idx = idx, na_pad = TRUE),
    runner(x2, lag = lag, idx = idx, f = which2, na_pad = TRUE))

  expect_equal(
    which_run(x2, k = 3, lag = 4, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, lag = 4, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, k = 3, lag = 4, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, lag = 4, idx = idx, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, k = 3, lag = -4, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, lag = -4, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, k = 3, lag = -4, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, lag = -4, idx = idx, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, k = k, lag = -4, idx = idx, na_pad = FALSE),
    runner(x2, k = k, lag = -4, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, k = k, lag = -4, idx = idx, na_pad = TRUE),
    runner(x2, k = k, lag = -4, idx = idx, f = which2, na_pad = TRUE))


  expect_equal(
    which_run(x2, k = 4, lag = lag, idx = idx, na_pad = FALSE),
    runner(x2, k = 4, lag = lag, idx = idx, f = which2, na_pad = FALSE))

  expect_equal(
    which_run(x2, k = 4, lag = lag, idx = idx, na_pad = TRUE),
    runner(x2, k = 4, lag = lag, idx = idx, f = which2, na_pad = TRUE))
})

test_that("Errors", {
  expect_error(which_run(x1, k = (1:999)), "length of k and length of x differs")
  expect_error(which_run(x1, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(which_run(x1, lag = (1:99)), "length of lag and length of x differs")
  expect_error(which_run(x1, lag = c(NA, lag[-1])), "Function doesn't accept NA values in lag vector")

  expect_error(which_run(x1, idx = (1:99)), "length of idx and length of x differs")
  expect_error(which_run(x1, idx = c(NA, 1:99)), "Function doesn't accept NA values in idx vector")
})
