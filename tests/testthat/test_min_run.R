context("Running min")
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)

k1 <- c(3,2,2,2,2)

test_that("min_run basic",{
  for(i in 1:15)
    expect_equal(
      min_run(x1)[i] ,
      min(x1[1:i])
    )
})

test_that("min_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      min_run(x2, na_rm = T)[i] ,
      min(x2[1:i], na.rm=T)
    )
})

test_that("min_run with na_rm=F na_fill=T", {
  for(i in 1:15)
    expect_equal(
      min_run(x2, na_rm = F )[i] ,
      min(x2[1:i], na.rm = F)
    )
})

test_that("min_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      min_run(x2, na_rm = T )[i] ,
      min(x2[1:i] , na.rm = T)
    )
})


test_that("min_run with na_rm=T k=4", {
  for(i in 1:15)
    expect_equal(
      min_run(x2, na_rm = T, k=4)[i] ,
      min(x2[pmax(i-4+1,1):i], na.rm=T)
    )
})



test_that("min_run pads NA's", {
  expect_identical(
    min_run( x2, na_pad=T,k=3 ),
    c( NA,NA,1,1,1,1,1,1,1,1,1,1,1,1,1)
  )
})

test_that("Error handling in min_run",{
  expect_error(min_run(x2, k=c(2,2,2,2,NA)))
  expect_error(min_run(x2, k=c(2,2,2,2,2,2)))
})
