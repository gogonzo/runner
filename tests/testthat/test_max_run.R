context("Running max")
suppressWarnings(RNGversion("3.5.0"))
set.seed(11)
x1 <- sample(c(1,2,3), 15, replace=T)
x2 <- sample(c(NA,1,2,3), 15, replace=T)
k  <- sample(1:15,15, replace=T)
idx <- function(i,k)
  ifelse( (i-k+1)<1,1,i-k+1)

test_that("max_run basic",{
  for(i in 1:15)
    expect_equal(
      max_run(x1)[i] ,
      max(x1[1:i])
    )
})

test_that("max_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      max_run(x2, na_rm = T)[i] ,
      max(x2[1:i], na.rm=T)
    )
})

test_that("max_run with na_rm=F", {
  for(i in 1:15)
    expect_equal(
      max_run(x2, na_rm = F )[i] ,
      max(x2[1:i])
    )
})


test_that("max_run with na_rm=T k=4", {
  for(i in 1:15)
    expect_equal(
      max_run(x2, na_rm = T, k=4)[i] ,
      max(x2[pmax(i-4+1,1):i], na.rm=T)
    )
})

test_that("max_run with na_rm=F k=4", {
  for(i in 1:15)
    expect_equal(
      max_run(x2, k=4, na_rm = F )[i] ,
      max(x2[pmax(i-4+1,1):i], na.rm = F)
    )
})

test_that("max_run pads NA's", {
  expect_identical(
    max_run( x2, na_pad=T,k=3 ),
    c( NA,NA,2,1,1,1,2,2,2,1,1,1,1,1,1)
  )
})

test_that("varying window", {
  for(i in 1:10)
    expect_equal(
      max_run(x1,k=k)[i],
      max(x1[idx(i,k[i]):i], na.rm=T)
    )

})


test_that("max_run with idx++ same as max_run with windows",{
  expect_identical( max_run(x1,k=3) , max_run(x1,k=3, idx=1:15) )
  expect_identical( max_run(x1,k=k) , max_run(x1,k=k, idx=1:15) )
})

test_that("max_run with idx",{
  x11 <- rep(NA, 15)
  x22 <- rep(NA, 15)
  idx <- cumsum(sample(c(1,2,3,4), 15, replace=T))

  for(i in 1:15)
    for(j in i:1)
      if(idx[j] >= (idx[i]-2)){
        x11[i] <- max(x1[j:i])
      } else {
        break;
      }


  for(i in 1:15)
    for(j in i:1)
      if(idx[j] >= (idx[i]-(k[i]-1))){
        x22[i] <- max(x1[j:i])
      } else {
        break;
      }

  expect_identical(max_run(x1, k=3, idx=idx), x11)
  expect_identical(max_run(x1, k=k, idx=idx), x22)

})


test_that("Error handling in max_run",{
  expect_error(max_run(x2, k=c(2,2,2,2,NA)))
  expect_error(max_run(x2, k=c(2,2,2,2,2,2)))
})


