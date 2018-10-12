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

test_that("Error handling in max_run",{
  expect_error(window_run(x2, k=c(2,2,2,2,2,2,2,2,2,NA)))
  expect_error(window_run(x2, k=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)))
})
