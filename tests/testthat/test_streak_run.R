context("Streak Length")
x1 <- c("a","a","b","b","a","a","a")
x2 <- c(NA,"a",NA,NA,NA,"b","a","b","b","a")
k1 <- c(3,2,2,2,2,2,1)

test_that("streak_run calculates consecutive streak", {
  expect_identical(streak_run(x1), as.integer(c(1,2,1,2,1,2,3)))
})

test_that("streak_run handles windowing", {
  expect_identical(streak_run(x1, k=2), as.integer(c(1,2,1,2,1,2,2)) )
  expect_identical(streak_run(x1, k=k1), as.integer(c(1,2,1,2,1,2,1)) )
})

test_that("streak_run handles NA's", {
  expect_identical(streak_run(x2), as.integer(c(NA,1,NA,NA,NA,1,1,1,2,1)) )
  expect_identical(streak_run(x2,na_pad=T,k=3), as.integer(c(NA,NA,NA,NA,NA,1,1,1,2,1)) )
})

test_that("Error in streak_run",{
  expect_error(streak_run(x1, k=c(2,2,2,2,NA,2,2)))
  expect_error(streak_run(x1, k=c(2,2,2,2,2,2)))
})

