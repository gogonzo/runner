context("Streak Length")
set.seed(11)
x1 <- sample(c("a","b"),15,replace=T)
x2 <- sample(c(NA_character_,"a","b"),15,replace=T)
k1 <- sample(1:4,15,replace=T)

test_that("streak_run calculates consecutive streak of any input type", {
  expect_identical(
    streak_run(x1),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3, 1, 2, 3))
  )
})

test_that("streak_run handles windowing", {
  expect_identical(
    streak_run(x1, k=2),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2))
  )
  expect_identical(
    streak_run(x1, k=k1),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2))
  )
})

test_that("streak_run handles NA's", {
  expect_identical(
    streak_run(x2),
    as.integer(c(1,2,NA,NA,1,NA,1,1,2,NA,1,2,NA,NA,1))
  )
  expect_identical(
    streak_run(x2,na_pad=T,k=3),
    as.integer(c(NA,NA,NA,NA,NA,NA,NA,1,2,NA,NA,NA,NA,NA,NA))
  )
})

test_that("Error in streak_run",{
  expect_error(streak_run(x1, k=c(2,2,2,2,NA,2,2)))
  expect_error(streak_run(x1, k=c(2,2,2,2,2,2)))
})


