context("Running length")
k  <- sample(1:5,10, replace=T)
idx <- cumsum(sample(c(1,2,3), 10, replace=T))

test_that("length_run constant k",{
  x1 <- rep(NA, 10)
  x2 <- rep(NA, 10)

  for(i in 1:10)
    for(j in i:1)
      if(idx[j] <= (idx[i]-3)){
        x1[i] <- i - j
        break
      }


  for(i in 1:10)
    for(j in i:1)
      if(idx[j] <= (idx[i]-k[i])){
        x2[i] <- i - j
        break
      }

  expect_identical( length_run(k=3, idx = idx), x1 )
  expect_identical( length_run(k=k, idx = idx), x2 )
})
