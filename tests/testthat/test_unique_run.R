context("Running unique")
set.seed(11)
x1 <- 1:10
x2 <- letters[1:10]
k <- sample(1:10, 10, replace=TRUE)
idx <- function(i,k)
  ifelse( (i-k+1)<1,1,i-k+1)


test_that("unique_run k=constant",{
  for(i in 1:10)
    expect_equal(
      sort(unique_run(x1,k=2)[i][[1]]),
      sort(unique(x1[idx(i,2):i]))
    )

  for(i in 1:10)
    expect_equal(
      sort(unique_run(x2,k=2)[i][[1]]),
      sort(unique(x2[idx(i,2):i]))
    )

  for(i in 1:10)
    expect_equal(
      sort(unique_run(as.character(x2),k=2)[i][[1]]),
      sort(unique(as.character(x2[idx(i,2):i])))
    )


  for(i in 1:10)
    expect_equal(
      sort(unique_run(as.numeric(x1),k=2)[i][[1]]),
      sort(unique(as.numeric(x1[idx(i,2):i])))
    )

})

test_that("unique_run with k varying", {
  for(i in 1:10)
    expect_equal(
      sort(unique_run(x2,k=k)[i][[1]]),
      sort(unique(x2[idx(i,k[i]):i]))
    )
})

test_that("unique_run with idx++ same as unique_run with windows",{
  expect_identical( unique_run(x1,k=3) , unique_run(x1,k=3, idx=1:10) )
  expect_identical( unique_run(x1,k=k) , unique_run(x1,k=k, idx=1:10) )
})

test_that("unique_run with idx",{
  x11 <- list()
  x22 <- list()
  idx <- cumsum(sample(c(1,2,3,4), 10, replace=T))

  for(i in 1:10)
    for(j in i:1)
      if(idx[j] >= (idx[i]-2)){
        x11[[i]] <- sort(unique(x1[j:i]))
      } else {
        break;
      }


  for(i in 1:10)
    for(j in i:1)
      if(idx[j] >= (idx[i]-(k[i]-1))){
        x22[[i]] <- sort(unique(x1[j:i]))
      } else {
        break;
      }

  expect_identical(lapply(unique_run(x1, k=3, idx=idx),sort), x11)
  expect_identical(lapply(unique_run(x1, k=k, idx=idx),sort), x22)

})



test_that("Error handling in max_run",{
  expect_error(unique_run(x2, k=c(2,2,2,2,2,2,2,2,2,NA)))
  expect_error(unique_run(x2, k=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)))
})
