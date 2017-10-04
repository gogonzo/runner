context("Running max")
x1 <- c(-1,-1,0,2,0,-2,1,0)
x2 <- c(NA,NA,-1,-1,NA,NA,2,0,-1.1,0,0,NA,1)

test_that("max_run handles zeros", {
  expect_identical(max_run(x1), c(-1,-1,0,2,2,2,2,2))
  expect_identical(max_run( x1 )[4], max(x1[1:4]))

})

test_that("max_run windowing", {
  expect_identical( max_run(x2,k=4), c(NA,NA,-1,-1,-1,-1,2,2,2,2,0,0,1) )
  expect_identical(
    min_run( x2,k=4 )[7],
    min(x2[4:7], na.rm=T)
  )

})


test_that("max_run handles NA's", {
  expect_identical(max_run(x2), c(NA,NA,-1,-1,-1,-1,2,2,2,2,2,2,2))
  expect_identical(max_run(x2,na_rm=F), c(NA,NA,-1,-1,NA,NA,2,2,2,2,2,NA,2))

  expect_identical( max_run(x2,na_rm=F,k=4), c(NA,NA,-1,-1,NA,NA,2,2,2,2,0,NA,1))
  expect_identical( max_run(x2,na_pad=T,na_rm=F,4), c(NA,NA,NA,-1,NA,NA,2,2,2,2,0,NA,1))
})

test_that("Error handling in max_run",{
  expect_error(max_run(x1, k=c(2,2,2,2,NA)))
  expect_error(max_run(x1, k=c(2,2,2,2,2,2)))
})
