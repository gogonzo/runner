context("Running window")
set.seed(11)
x1 <- 1:10
x2 <- letters[1:10]

k <- sample(1:10, 10, replace=TRUE)
idx <- function(i,k)
  ifelse( (i-k+1)<1,1,i-k+1)


test_that("window_run k=constant",{
  for(i in 1:10)
    expect_equal(
     window_run(x1,k=2)[i][[1]],
     x1[idx(i,2):i]
    )

  for(i in 1:10)
    expect_equal(
      window_run(x2,k=2)[i][[1]],
      x2[idx(i,2):i]
    )

  for(i in 1:10)
    expect_equal(
      window_run(as.character(x2),k=2)[i][[1]],
      as.character(x2[idx(i,2):i])
    )


  for(i in 1:10)
    expect_equal(
      window_run(as.numeric(x1),k=2)[i][[1]],
      as.numeric(x1[idx(i,2):i])
    )



})

test_that("window_run with k varying", {
  for(i in 1:10)
    expect_equal(
      window_run(x2,k=k)[i][[1]],
      x2[idx(i,k[i]):i]
    )
})


test_that("window_run with idx++ same as window_run with windows",{
  expect_identical( window_run(x1,k=3) , window_run(x1,k=3, idx=1:10) )
  expect_identical( window_run(x1,k=k) , window_run(x1,k=k, idx=1:10) )
})

test_that("unique_run with idx",{
  x11 <- list()
  x22 <- list()
  idx <- cumsum(sample(c(1,2,3,4), 10, replace=T))

  for(i in 1:10)
    for(j in i:1)
      if(idx[j] >= (idx[i]-2)){
        x11[[i]] <- x1[j:i]
      } else {
        break;
      }


  for(i in 1:10)
    for(j in i:1)
      if(idx[j] >= (idx[i]-(k[i]-1))){
        x22[[i]] <- x1[j:i]
      } else {
        break;
      }

  expect_identical(window_run(x1, k=3, idx=idx), x11)
  expect_identical(window_run(x1, k=k, idx=idx), x22)
})



test_that("Error handling in max_run",{
  expect_error(window_run(x2, k=c(2,2,2,2,2,2,2,2,2,NA)))
  expect_error(window_run(x2, k=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)))
})
