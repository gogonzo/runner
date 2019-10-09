context("Fill run")

test_that("repeat run works for vectors on any type", {
  expect_identical(
    as.integer(fill_run(c(NA,NA,1:10, NA, NA), run_for_first= TRUE)),
    as.integer(c(1,1,1:10,10,10))
  )
  expect_identical(
    fill_run(as.character(c(NA,NA,1:10, NA, NA)), run_for_first = TRUE),
    as.character(c(1,1,1:10,10,10))
  )

  expect_identical(
    fill_run(as.complex(c(NA,NA,1:10, NA, NA)), run_for_first = TRUE),
    as.complex(c(1,1,1:10,10,10))
  )

  expect_warning(fill_run( c(NA,NA,NA,NA)))

  expect_equal(
    fill_run(factor(c(NA,NA,1,2,NA,NA,2,2,NA,NA,1, NA, NA)), run_for_first = TRUE, only_within = TRUE),
    c(1,1,1,2,2,2,2,2,NA,NA,1,NA,NA)
  )

  expect_equal(
    fill_run(as.complex(c(NA,NA,1,2,NA,NA,2,2,NA,NA,1, NA, NA)), run_for_first = TRUE, only_within = TRUE),
    as.complex(c(1,1,1,2,2,2,2,2,NA,NA,1,NA,NA))
  )

  expect_equal(
    fill_run(c(NA,NA,T,T,NA, NA, T, T, NA, NA, T, NA, NA), run_for_first = TRUE, only_within = TRUE),
    c(T,T,T,T,T,T,T,T,T,T,T,NA,NA)
  )

})

