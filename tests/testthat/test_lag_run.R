context("Running lag")
x <- runif(10)


test_that("lag_run basic - different types",{
  expect_identical(
   lag_run(x),
   c(NA, x[-10])
  )

  expect_identical(
    lag_run(as.character(x)),
    as.character(c(NA, x[-10]))
  )

  expect_identical(
    lag_run(as.integer(round(x))),
    as.integer(round(c(NA, x[-10])))
  )

})


test_that("lag_run constant window",{
  x2 <- rep(NA, 10)
  for(i in 4:10) x2[i] <- x[i-3]

  expect_identical( lag_run(x, k=3), x2 )
})

test_that("lag_run moving window",{
  x2 <- rep(NA, 10)
  k  <- sample(1:5,10, replace=T)
  for(i in 1:10)
    x2[i] <- ifelse(i>k[i],x[i-k[i]], NA)

  expect_identical( lag_run(x, k=k), x2 )
})

test_that("lag_run moving idx window",{
  x1 <- rep(NA, 10)
  x2 <- rep(NA, 10)
  k  <- sample(1:5,10, replace=T)
  idx <- cumsum(sample(c(1,2,3), 10, replace=T))

  for(i in 1:10)
    for(j in i:1)
      if(idx[j] <= (idx[i]-3)){
        x1[i] <- x[j]
        break
      }


  for(i in 1:10)
    for(j in i:1)
      if(idx[j] <= (idx[i]-k[i])){
        x2[i] <- x[j]
        break
      }



  expect_identical( lag_run(x, k=3, idx = idx), x1 )
  expect_identical( lag_run(x, k=k, idx = idx), x2 )
})
