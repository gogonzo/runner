context("Running sum")
set.seed(11)
x1 <- rnorm(15)
x2 <- sample(c(rep(NA,5),rnorm(15)), 15, replace=TRUE)
k <- sample(1:15, 15, replace=TRUE)


test_that("sum_run basic",{
  for(i in 1:15)
    expect_equal(
      sum_run(x1)[i] ,
      sum(x1[1:i])
    )
})

test_that("sum_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      sum_run(x2, na_rm = T)[i] ,
      as.numeric( ifelse(all(is.na(x2[1:i])),NA,sum(x2[1:i], na.rm=T)) )
    )
})

test_that("sum_run with na_rm=F na_fill=T", {
  for(i in 1:15)
    expect_equal(
      sum_run(x2, na_rm = F )[i] ,
      sum(x2[1:i], na.rm = F)
    )
})


test_that("sum_run with na_rm=T k=4", {
  for(i in 1:15)
    expect_equal(
      sum_run(x2, na_rm = T, k=4)[i] ,
      as.numeric( ifelse(all(is.na(x2[pmax(i-4+1,1):i])),NA, sum(x2[pmax(i-4+1,1):i], na.rm=T)))
    )
})



test_that("sum_run pads NA's", {
  expect_identical(
    sum_run( x2, na_pad=T,k=3 ),
    c( NA,NA,1,1,1,1,1,1,1,1,1,1,1,1,1)
  )
})

test_that("Error handling in sum_run",{
  expect_error(sum_run(x2, k=c(2,2,2,2,NA)))
  expect_error(sum_run(x2, k=c(2,2,2,2,2,2)))
})
