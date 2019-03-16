context("Streak Length")
suppressWarnings(RNGversion("3.5.0"))
set.seed(11)
x1 <- sample(c("a","b"),15,replace=T)
x2 <- sample(c(NA_character_,"a","b"),15,replace=T)
k1 <- sample(1:4,15,replace=T)

test_that("streak_run calculates consecutive streak of any input type", {
  expect_identical(
    streak_run(x1),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3, 1, 2, 3))
  )

  expect_equal(
    streak_run(as.numeric(as.factor(x1))),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3, 1, 2, 3))
  )

  expect_identical(
    streak_run(as.character(x1)),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3, 1, 2, 3))
  )

  expect_identical(
    streak_run(as.factor(x1)),
    as.integer(c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 3, 1, 2, 3))
  )

  expect_identical(
    streak_run(c(T,T,T,T,F,T)),
    as.integer(c(1,2,3,4,1,1))
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
    as.integer(c(1,2,2,2,3,3,1,1,2,2,3,4,4,4,5))
  )
  expect_identical(
    streak_run(x2,na_pad=T,k=3),
    as.integer(c(NA,NA,2,1,1,1,1,1,2,2,2,2,2,1,1))
  )
})

test_that("sum_run with idx++ same as sum_run with windows",{
  expect_identical( streak_run(x1,k=3) , streak_run(x1,k=3, idx=1:15) )
  expect_identical( streak_run(x1,k=k1) , streak_run(x1,k=k1, idx=1:15) )


  expect_identical( streak_run(as.factor(x1),k=3) , streak_run(as.factor(x1),k=3, idx=1:15) )
  expect_identical( streak_run(as.factor(x1),k=k1), streak_run(as.factor(x1),k=k1, idx=1:15) )
})


test_that("Error in streak_run",{
  expect_error(streak_run(x1, k=c(2,2,2,2,NA,2,2)))
  expect_error(streak_run(x1, k=c(2,2,2,2,2,2)))
})


