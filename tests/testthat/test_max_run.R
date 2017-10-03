context("Running max")
x1 <- c(-1,-1,0,2,0,-2,1,0)
x2 <- c(NA,NA,-1,-1,NA,NA,2,0,-1.1,0,0,NA,1)

test_that("max_run handles zeros", {
  expect_identical(max_run(x1), c(-1,-1,-1,-1,-1,-2,-2,-2))
})

test_that("max_run windowing", {
  expect_identical( max_run(x2,k=4), c(NA,NA, -1,-1, -1,-1, -1, 0, -1.1,-1.1,-1.1,-1.1,0) )
})


test_that("max_run handles NA's", {
  expect_identical(max_run(x2), c(NA,NA, -1,-1,-1,-1,-1,-1, -1.1,-1.1,-1.1,-1.1,-1.1))
  expect_identical(max_run(x2,na_rm=F), c(NA,NA, -1,-1,NA,NA,-1,-1, -1.1,-1.1,-1.1,NA,-1.1))

  expect_identical( max_run(x2,na_rm=F,k=4), c(NA,NA, -1,-1, NA,NA, -1, 0, -1.1,-1.1,-1.1,NA,0))
  expect_identical( max_run(x2,na_pad=T,na_rm=F,4), c(NA,NA, NA,-1, NA,NA, -1, 0, -1.1,-1.1,-1.1,NA,0))
})
