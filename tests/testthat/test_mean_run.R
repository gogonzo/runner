context("Running mean")
set.seed(11)
x1 <- rnorm(15)
x2 <- sample(c(rep(NA,5),rnorm(15)), 15, replace=TRUE)
k <- sample(1:15, 15, replace=TRUE)
test_that("mean_run basic",{
  for(i in 1:15)
    expect_equal(
      mean_run(x1)[i] ,
      mean(x1[1:i])
    )
})

test_that("mean_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      mean_run(x2, na_rm = T)[i] ,
      mean(x2[1:i], na.rm=T)
    )
})

test_that("mean_run with na_rm=F na_fill=T", {
  for(i in 1:15)
    expect_equal(
      mean_run(x2, na_rm = F )[i] ,
      mean(x2[1:i], na.rm = F)
    )
})

test_that("mean_run with na_rm=T", {
  for(i in 1:15)
    expect_equal(
      mean_run(x2, na_rm = T )[i] ,
      mean(x2[1:i] , na.rm = T)
    )
})


test_that("mean_run with na_rm=T k=4", {
  for(i in 1:15)
    expect_equal(
      mean_run(x2, na_rm = T, k=4)[i] ,
      mean(x2[pmax(i-4+1,1):i], na.rm=T)
    )
})


test_that("sum_run with na_rm=T varying k", {
  for(i in 1:15)
    expect_equal(
      mean_run(x2, na_rm = T, k=k)[i] ,
      as.numeric( ifelse(all(is.na(x2[pmax(i-k[i]+1,1):i])),NA, mean(x2[pmax(i-k[i]+1,1):i], na.rm=T)))
    )
})


test_that("mean_run with idx++ same as mean_run with windows",{
  expect_identical( mean_run(x1,k=3) , mean_run(x1,k=3, idx=1:15) )
  expect_identical( mean_run(x1,k=k) , mean_run(x1,k=k, idx=1:15) )
})

test_that("mean_run with idx",{
  x11 <- rep(NA, 15)
  x22 <- rep(NA, 15)
  idx <- cumsum(sample(c(1,2,3,4), 15, replace=T))

  for(i in 1:15)
    for(j in i:1)
      if(idx[j] >= (idx[i]-2)){
        x11[i] <- mean(x1[j:i])
      } else {
        break;
      }


  for(i in 1:15)
    for(j in i:1)
      if(idx[j] >= (idx[i]-(k[i]-1))){
        x22[i] <- mean(x1[j:i])
      } else {
        break;
      }

  expect_equal(mean_run(x1, k=3, idx=idx), x11)
  expect_equal(mean_run(x1, k=k, idx=idx), x22)

})


test_that("Error handling in mean_run",{
  expect_error(mean_run(x2, k=c(2,2,2,2,NA)))
  expect_error(mean_run(x2, k=c(2,2,2,2,2,2)))
})

