context("Streak Length")
suppressWarnings(RNGversion("3.5.0"))
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)
k1  <- sample(1:15,15, replace=T)
idx <- cumsum(sample(c(1,2,3,4), 15, replace = TRUE))
lag <- sample(0:3, 15, replace = TRUE)


test_that("streak_run calculates consecutive streak of any input type", {
  expect_identical(
    streak_run(x1),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3))
  )

  expect_equal(
    streak_run(as.numeric(as.factor(x1))),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3))
  )

  expect_identical(
    streak_run(as.character(x1)),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3))
  )

  expect_identical(
    streak_run(as.factor(x1)),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3))
  )

  expect_identical(
    streak_run(c(T, T, T, T, F, T)),
    as.integer(c(1, 2, 3, 4, 1, 1))
  )

})

test_that("Streak lag", {
  expect_identical(
    streak_run(c(T, T, T, T, F, T), lag = 1),
    as.integer(c(NA, 1, 2, 3, 4, 1))
  )

  expect_identical(
    streak_run(x1, lag = 3),
    as.integer(c(NA, NA, NA, 1, 2,
                 1, 1, 2, 1, 1,
                 2, 1, 1, 2, 1))
  )
})

test_that("Streak negative lag", {
  expect_identical(
    streak_run(c(T, T, T, T, F, T), lag = -1),
    as.integer(c(NA, 1, 2, 3, 4, 1))
  )

  expect_identical(
    streak_run(x1, lag = -3),
    as.integer(c(NA, NA, NA, 1, 2,
                 1, 1, 2, 1, 1,
                 2, 1, 1, 2, 1))
  )
})


test_that("streak_run handles windowing", {
  expect_identical(
    streak_run(x1, k = 2),
    as.integer(c(1, 2, 1, 1, 2,
                 1, 1, 2, 1, 1,
                 2, 1, 1, 2, 2))
  )
  expect_identical(
    streak_run(x1, k = k1),
    as.integer(c(1, 2, 1, 1, 2,
                 1, 1, 2, 1, 1,
                 2, 1, 1, 2, 3))
  )
})

test_that("streak_run handles NA's", {
  expect_identical(
    streak_run(x2, na_rm = TRUE),
    as.integer(c(1, 1, 2, 2, 3,
                 3, 1, 1, 2, 2,
                 3, 4, 4, 4, 5))
  )

  expect_identical(
    streak_run(x2, na_rm = FALSE),
    as.integer(c(1, 1, 2, NA, 1,
                 NA, 1, 1, 2, NA,
                 1, 2, NA, NA, 1))
  )

  expect_identical(
    streak_run(x2, na_pad = TRUE, k = 3),
    as.integer(c(NA, NA, 2, 2, 2,
                 1, 1, 1, 2, 2,
                 2, 2, 2, 1, 1))
  )
})

test_that("sum_run with idx same as sum_run with windows",{
  expect_identical(streak_run(x1, k =  3),
                   streak_run(x1, k =  3, idx = seq_len(15)))
  expect_identical(streak_run(x1, k = k1),
                   streak_run(x1, k = k1, idx = seq_len(15)))

  expect_identical(streak_run(as.factor(x1), k = 3) ,
                   streak_run(as.factor(x1), k = 3, idx = 1:15))
  expect_identical(streak_run(as.factor(x1), k = k1),
                   streak_run(as.factor(x1), k = k1, idx = 1:15))
})

test_that("sum_run with idx",{
  expect_identical(streak_run(x1, k =  4, idx = idx),
                   as.integer(c(1, 2, 1, 1, 1,
                                1, 1, 2, 1, 1,
                                2, 1, 1, 2, 2)))


  expect_identical(streak_run(x1, k =  5, lag = 2, idx = idx),
                   as.integer(c(NA, 1, 1, 1, 1,
                                1, 1, 1, 2, 2,
                                1, 2, 1, 1, 2)))



  expect_identical(streak_run(x1, k = 5, lag = 2, idx = idx),
                   streak_run(x1, k = rep(5, 15), lag = rep(2, 15), idx = idx))

})




test_that("Errors", {
  expect_error(streak_run(list(x1), k = 5), "Invalid data type")

  expect_error(streak_run(x1, k = (1:9)), "length of k and length of x differs")
  expect_error(streak_run(x1, k = c(NA, k1[-1])), "Function doesn't accept NA values in k vector")

  expect_error(streak_run(x1, lag = (1:9)), "length of lag and length of x differs")
  expect_error(streak_run(x1, lag = c(NA, k1[-1])), "Function doesn't accept NA values in lag vector")
  expect_warning(streak_run(x1, lag = 15), "lag value is greater than length of x")

  expect_error(streak_run(x1, idx = (1:9)), "length of idx and length of x differs")
  expect_error(streak_run(x1, idx = c(NA, 1:14)), "Function doesn't accept NA values in idx vector")
})


