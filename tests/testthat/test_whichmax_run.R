context("Running whichmax")
set.seed(11)
x1 <- c(1, 1, 2, 1, 1, 3, 1, 1, 3, 1, 1, 2, 3, 3, 3)
x2 <- c(2, 1, 1, NA, 3, 2, 1, NA, 1, NA, NA, NA, 1, 2, 1)
k  <- c(5, 1, 8, 1, 1, 15, 2, 5, 14, 2, 3, 7, 14, 13, 12)

test_that("max_run basic",{
  expect_equal(
    whichmax_run(x1, which="first"),
    c(1, 1, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
  )

  expect_equal(
    whichmax_run(x1, which="last"),
    c(1, 2, 3, 3, 3, 6, 6, 6, 9, 9, 9, 9, 13, 14, 15)
  )

})

test_that("whichmax_run with missings", {
  expect_equal(
    whichmax_run(x2, na_rm = T, which="first") ,
    c(1,1,1,1,5,5,5,5,5,5,5,5,5,5,5)
  )

  expect_equal(
    whichmax_run(x2, na_rm = T, which="last") ,
    c(1,1,1,1,5,5,5,5,5,5,5,5,5,5,5)
  )

  expect_equal(
    whichmax_run(x2, na_rm = F, which="first") ,
    c(1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  )
})

test_that("whichmax_run with k=3", {
  expect_equal(
    whichmax_run(x2 , k=3, na_rm = T, which="first"),
    c(1,1,1,2,5,5,5,6,7,9,9,NA,13,14,14)
  )
  expect_equal(
    whichmax_run(x2, k=3, na_rm = T, which="last"),
    c(1,1,1,3,5,5,5,6,9,9,9,NA,13,14,14)
  )

  expect_equal(
    whichmax_run(x2 , k=3, na_rm = F, which="first"),
    c(1, 1, 1, NA, NA, NA, 5, NA, NA, NA, NA, NA, NA, NA, 14)
  )
})

test_that("whichmax_run with varying k", {
  expect_equal(
    whichmax_run( x2 , k = k , na_rm = T, which = "last"),
    c(1, 2, 1, NA, 5,   5, 6, 5, 5, 9,    9, 6, 5, 5, 5)
  )

  expect_equal(
    whichmax_run(x2 , k=k, na_rm = F, which="first"),
    c(1, 2, 1, NA, 5, NA, 6, NA, NA, NA, NA, NA, NA, NA, NA)
  )

})

test_that("Error handling in max_run",{
  expect_error(whichmax_run(x2, k=c(2,2,2,2,2,2,2,NA,2,2,2,2,2,2,2)))
  expect_error(whichmax_run(x2, k=c(2,2,2,2,2,2)))
})
