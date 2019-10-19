context("Running which")
set.seed(11)
x1 <- sample(c(TRUE, FALSE, NA), 20, replace = TRUE)
x2 <- sample(c(TRUE, FALSE, NA), 20, replace = TRUE)
k <- sample(seq_len(20), 20, replace = TRUE)
which_test <- function(x, arg_which, na_rm) {
  if (!na_rm) {
    if (arg_which == "last") {
      ifelse(
        max(which(x)) > max(c(0, which(is.na(x)))),
        max(which(x)), NA
      )
    } else if (arg_which == "first") {
      ifelse(
        min(which(x)) > min(c(0, which(is.na(x)))),
        min(which(x)), NA
      )
    }
  } else {
    if (arg_which == "last") {
      max(which(x))
    } else if (arg_which == "first") {
      min(which(x))
    }
  }



}


test_that("which_run first and last", {
  expect_identical(
    which_run(x1, which = "first"),
    sapply(window_run(x1), function(x) which_test(x, "first", TRUE))
  )

  expect_identical(
    which_run(x1, which = "last"),
    sapply(window_run(x1), function(x) which_test(x, "last", TRUE))
  )

})

test_that("which with na_rm = FALSE", {
  expect_identical(
    which_run(x2, na_rm = FALSE),
    sapply(window_run(x2), function(x) which_test(x, "last", FALSE))
  )
  expect_identical(
    which_run(x2, na_rm = FALSE, which = "first"),
    sapply(window_run(x2), function(x) which_test(x, "first", FALSE))
  )
})

test_that("which with na_rm = TRUE k = 4", {
  idx <- as.integer(1:20)
  expect_identical(
    which_run(x2, na_rm = FALSE, which = "first", idx = idx),
    sapply(window_run(x2, idx = idx), function(x) which_test(x, "first", FALSE))
  )

  for(i in 1:20)
    expect_equal(
      which_run(x2, na_rm = T, k = 4, which = "first")[i] ,
      ifelse(is.finite(min( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T)),
             min( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T),
             NA_integer_)
    )
})

test_that("which with na_rm = FALSE k = 4", {
  idx <- 1:20
  for(i in 1:20)
    expect_equal(
      which_run(x2, na_rm = FALSE, k = 4)[i] ,
      ifelse(
        max( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T) >
        max( c(0,idx[ idx %in% (pmax(1,i-3):i) & is.na(x2) ]), na.rm=T),
        max( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T), NA_integer_
      )
    )

  for(i in 1:20)
    expect_equal(
      which_run(x2, na_rm = FALSE, k = 4, which = "first")[i] ,
      ifelse(
        min( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T) <
        min( c(Inf,idx[ idx %in% (pmax(1,i-3):i) & is.na(x2)]), na.rm=T),
        min( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T), NA_integer_
      )
    )
})

test_that("which with na_rm = TRUE k = k", {
  idx <- as.integer(1:20)
  for(i in 1:20)
    expect_equal(
      which_run(x2, na_rm = T, k=k)[i] ,
      ifelse(is.finite(max( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T)),
         max( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T),
         NA_integer_)
    )


  for(i in 1:20)
    expect_equal(
      which_run(x2, na_rm = T, k = k, which = "first")[i] ,
      ifelse(is.finite(min( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T)),
             min( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T),
             NA_integer_)
    )
})

test_that("which with sequential indexes equals non-indexed",{
  x <- sample(c(NA, F, T), 10, replace = TRUE)
  k <- sample(seq_len(10), 10, replace = TRUE)

  expect_identical(
    which_run(x, which = "last", idx = seq_len(10)),
    which_run(x, which = "last")
  )
  expect_identical(
    which_run(x, which = "last", k = 4, idx = seq_len(10) * 2 ),
    which_run(x, which = "last", k = 2)
  )
  expect_identical(
    which_run(x, which = "last", k = k * 2, idx = seq_len(10) * 2 ),
    which_run(x, which = "last", k = k )
  )

})

test_that("which for indexed window",{
  x <- c(NA,F, T, NA, F, F, T, T, NA, T, F , T)
  i <- c(1, 2, 3, 3,  3, 4, 6, 8, 9,  9, 13, 13)
  k <- sample(1:12,12,replace=T)

  expect_identical(
    which_run( x, which="last", idx = i, k = 3, na_rm = FALSE),
    as.integer(c(NA, NA, 3, NA, NA, NA, 7, 8, NA, 10, NA, 12))
  )
  expect_identical(
    which_run( x, which="first", idx = i, k = 2, na_rm = FALSE),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 8, NA, 12))
  )
  expect_identical(
    which_run( x, which="last", idx = i, k = 2, na_rm = TRUE),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 10, NA, 12))
  )

})


test_that("Errors", {

  expect_error(which_run(x1, k = (1:9)), "length of k and length of x differs")
  expect_error(which_run(x1, k = c(NA, k[-1])), "Function doesn't accept NA values in k vector")

  expect_error(which_run(x1, lag = (1:9)), "length of lag and length of x differs")
  expect_error(which_run(x1, lag = c(NA, k[-1])), "Function doesn't accept NA values in lag vector")
  expect_warning(which_run(x1, lag = 20), "lag value is greater than length of x")

  expect_error(which_run(x1, idx = (1:9)), "length of idx and length of x differs")
  expect_error(which_run(x1, idx = c(NA, 1:19)), "Function doesn't accept NA values in idx vector")

  expect_error(which_run(x1, which = "test"), "which value should be either")
})

