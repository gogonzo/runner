context("Running max")
x1 <- c( -1, -1, -2, 0.1, 2 )
x2 <- c( NA, -1.0, -1.0,NA, NA, 0.0, -2.1, NA, 2.0 )
k1 <- c(3,2,2,2,2)

test_that("min_run handles zeros", {
  expect_identical(min_run( x1 ), c( -1, -1, -2, -2, -2))
})

test_that("min_run handles windowing", {
  expect_identical(min_run( x1 , k = 2), c( -1, -1, -2, -2, 0.1) )
})

test_that("max_run handles windowing", {
  expect_identical( min_run( x1 , k = 3), c(-1,-1,-2,-2,-2))
})

test_that("min_run handles varying windowing", {
  expect_identical( min_run( x1 , k = k1), c(-1,-1,-2,-2,0.1))
})

test_that("min_run handles NA's default", {
  expect_identical(min_run( x2 ), c( NA, -1, -1, -1, -1,-1,-2.1,-2.1,-2.1))
})

test_that("min_run handles NA's na_rm=F", {
  expect_identical(min_run( x2, na_rm=F ), c( NA, -1, -1, NA, NA,-1,-2.1,NA,-2.1))
})

test_that("min_run pads NA's", {
  expect_identical(min_run( x2, na_pad=T,k=3 ), c( NA,NA,-1,-1,-1,0,-2.1,-2.1,-2.1))
})
