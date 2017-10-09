context("Running whichmin")
set.seed(11)
x1 <- c(1, 1, 2, 1, 1, 3, 1, 1, 3, 1, 1, 2, 3, 3, 3)
x2 <- c(2, 1, 1, NA, 3, 2, 1, NA, 1, NA, NA, NA, 1, 2, 1)
k  <- c(5, 1, 8, 1, 1, 15, 2, 5, 14, 2, 3, 7, 14, 13, 12)

test_that("min_run basic",{
  expect_equal(
    whichmin_run( x1 , which="first") ,
    rep(1, 15)
  )

  expect_equal(
    whichmin_run(x1, which="last"),
    c(1, 2, 2, 4, 5, 5, 7, 8, 8, 10, 11, 11, 11, 11, 11)
  )

})


test_that("whichmin_run with missings", {
  expect_equal(
    whichmin_run( x2, na_rm = T , which="first" ) ,
    c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  )

  expect_equal(
    whichmin_run(x2, na_rm = T, which="last") ,
    c( 1, 2, 3, 3, 3,  3, 7, 7, 9, 9,   9, 9, 13, 13, 15)
  )

  expect_equal(
    whichmin_run(x2, na_rm = F, which="first") ,
    c(1,2,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  )
})

test_that("whichmin_run with k=3", {
  expect_equal(
    whichmin_run(x2 , k=3, na_rm = T, which="first"),
    c(1,2,2,2,3  ,6,7,7,7,9,  9,NA,13,13,13)
  )
  expect_equal(
    whichmin_run(x2, k=3, na_rm = T, which="last"),
    c(1,2,3,3,3,  6,7,7,9,9,  9,NA,13,13,15)
  )

  expect_equal(
    whichmin_run(x2 , k=3, na_rm = F, which="first"),
    c(1,2,2,NA,NA,  NA,7,NA,NA,NA,  NA,NA,NA,NA,13)
  )
})

test_that("whichmin_run with varying k", {
  expect_equal(
    whichmin_run( x2 , k = k , na_rm = T, which = "last"),
    c(1,2,3,NA,5,  3,7,7,9,9,  9,9,13,13,15)
  )

  expect_equal(
    whichmin_run(x2 , k=k, na_rm = F, which="first"),
    c(1,2,2,NA,5,  NA,7,NA,NA,NA,  NA,NA,NA,NA,NA)
  )

})

test_that("Error handling in min_run",{
  expect_error(whichmin_run(x2, k=c(2,2,2,2,2,2,2,NA,2,2,2,2,2,2,2)))
  expect_error(whichmin_run(x2, k=c(2,2,2,2,2,2)))
})
