context("Fill run")

test_that("repeat run works for vectors on any type", {
  expect_identical(
    as.integer(fill_run(c(NA,NA,1:10, NA, NA), run_for_first=T)),
    as.integer(c(1,1,1:10,10,10))
  )
  expect_identical(
    as.character(fill_run(c(NA,NA,1:10, NA, NA), run_for_first=T)),
    as.character(c(1,1,1:10,10,10))
  )

  expect_identical(
    as.factor(fill_run(c(NA,NA,1:10, NA, NA), run_for_first=T)),
    as.factor(c(1,1,1:10,10,10))
  )

  expect_warning( fill_run( c(NA,NA,NA,NA) ) )

  expect_identical(
    as.factor(fill_run(c(NA,NA,1:10, NA, NA), run_for_first=F)),
    as.factor(c(NA,NA,1:10,10,10))
  )

  expect_identical(
    as.factor(fill_run(c(NA,NA,1,2,NA,NA,2,2,NA,NA,1, NA, NA), run_for_first=T,only_within = T)),
    as.factor(c(1,1,1,2,2,2,2,2,NA,NA,1,NA,NA))
  )

})
