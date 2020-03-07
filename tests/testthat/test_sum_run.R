context("Running sum")
set.seed(11)
x1 <- sample(c(1, 2, 3), 100, replace = TRUE)
x2 <- sample(c(NA, 1, 2, 3), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
sum2 <- function(x, na_rm = TRUE) {
  if (all(is.na(x))) return(NA) else sum(x, na.rm = na_rm)
}
ids <- sample(1:100, 10)

test_that("       |--------]------->", {
  expect_identical(
    sum_run(x2),
    runner(x2, f = sum2)
  )

  expect_identical(
    sum_run(x2, na_pad = TRUE),
    runner(x2, f = sum2, na_pad = TRUE)
  )

  expect_identical(
    sum_run(x2, na_rm = FALSE),
    runner(x2, function(x) sum2(x, na_rm = FALSE))
  )



  expect_identical(
    sum_run(x2)[ids],
    sum_run(x2, at = seq_along(x2)[ids])
  )

  expect_identical(
    sum_run(x2, na_pad = TRUE)[ids],
    sum_run(x2, at = seq_along(x2)[ids], na_pad = TRUE)
  )

  expect_identical(
    sum_run(x2, na_rm = FALSE)[ids],
    sum_run(x2, at = seq_along(x2)[ids], na_rm = FALSE)
  )
})

test_that("   [...|----]---+------->", {
  expect_equal(
    sum_run(x2, lag = 3),
    runner(x2, lag = 3, f = sum2))

  expect_equal(
    sum_run(x2, lag = 3, na_pad = TRUE),
    runner(x2, lag = 3, f = sum2, na_pad = TRUE))



  expect_equal(
    sum_run(x2, lag = 3)[ids],
    sum_run(x2, lag = 3, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, lag = 3, na_pad = TRUE)[ids],
    sum_run(x2, lag = 3, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("       |--------+---]--->", {
  expect_equal(
    sum_run(x2, lag = -3),
    runner(x2, lag = -3, f = sum2))

  expect_equal(
    sum_run(x2, lag = -3, na_pad = TRUE),
    runner(x2, lag = -3, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, lag = -3)[ids],
    sum_run(x2, lag = -3, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, lag = -3, na_pad = TRUE)[ids],
    sum_run(x2, lag = -3, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("  [...]|--------+------->", {
  expect_equal(
    sum_run(x2, lag = 100),
    runner(x2, lag = 100, f = sum2, type = "numeric"))

  expect_equal(
    sum_run(x2, lag = 100, na_pad = TRUE),
    runner(x2, lag = 100, f = sum2, na_pad = TRUE, type = "numeric"))

  expect_equal(
    sum_run(x2, lag = -100),
    runner(x2, lag = -100, f = sum2))

  expect_equal(
    sum_run(x2, lag = -100, na_pad = TRUE),
    runner(x2, lag = -100, f = sum2, na_pad = TRUE, type = "numeric"))



  expect_equal(
    sum_run(x2, lag = 100)[ids],
    sum_run(x2, lag = 100, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, lag = 100, na_pad = TRUE)[ids],
    sum_run(x2, lag = 100, at = seq_along(x2), na_pad = TRUE)[ids])

  expect_equal(
    sum_run(x2, lag = -100)[ids],
    sum_run(x2, lag = -100, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, lag = -100, na_pad = TRUE)[ids],
    sum_run(x2, lag = -100, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("       |----[...]------->", {
  expect_equal(
    sum_run(x2, k = 3),
    runner(x2, k = 3, f = sum2))

  expect_equal(
    sum_run(x2, k = 3, na_pad = TRUE),
    runner(x2, k = 3, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, k = 3)[ids],
    sum_run(x2, k = 3, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 3, na_pad = TRUE)[ids],
    sum_run(x2, k = 3, at = seq_along(x2)[ids], na_pad = TRUE))

})

test_that("       [...|--------+-------[...]", {
  expect_equal(
    sum_run(x2, k = 1),
    runner(x2, k = 1, f = sum2))

  expect_equal(
    sum_run(x2, k = 1, na_pad = TRUE),
    runner(x2, k = 1, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 99),
    runner(x2, k = 99, f = sum2))

  expect_equal(
    sum_run(x2, k = 99, na_pad = TRUE),
    runner(x2, k = 99, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 100),
    runner(x2, k = 100, f = sum2))

  expect_equal(
    sum_run(x2, k = 100, na_pad = TRUE),
    runner(x2, k = 100, f = sum2, na_pad = TRUE))



  expect_equal(
    sum_run(x2, k = 1)[ids],
    sum_run(x2, k = 1, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 1, na_pad = TRUE)[ids],
    sum_run(x2, k = 1, at = seq_along(x2)[ids], na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 99)[ids],
    sum_run(x2, k = 99, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 99, na_pad = TRUE)[ids],
    sum_run(x2, k = 99, at = seq_along(x2)[ids], na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 100)[ids],
    sum_run(x2, k = 100, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 100, na_pad = TRUE)[ids],
    sum_run(x2, k = 100, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("       [...|----]---+------->", {
  expect_equal(
    sum_run(x2, k = 5, lag = 3),
    runner(x2, k = 5, lag = 3, f = sum2))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_pad = TRUE),
    runner(x2, k = 5, lag = 3, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_rm = FALSE),
    runner(x2, k = 5, lag = 3, f = sum, type = "numeric"))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_pad = TRUE, na_rm = FALSE),
    runner(x2, k = 5, lag = 3, f = sum, na_pad = TRUE, type = "numeric"))



  expect_equal(
    sum_run(x2, k = 5, lag = 3)[ids],
    sum_run(x2, k = 5, lag = 3, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_pad = TRUE)[ids],
    sum_run(x2, k = 5, lag = 3, at = seq_along(x2)[ids], na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_rm = FALSE)[ids],
    sum_run(x2, k = 5, lag = 3, at = seq_along(x2)[ids], na_rm = FALSE))

  expect_equal(
    sum_run(x2, k = 5, lag = 3, na_pad = TRUE, na_rm = FALSE)[ids],
    sum_run(x2, k = 5, lag = 3, at = seq_along(x2)[ids], na_pad = TRUE, na_rm = FALSE))
})

test_that("       |-----[--+---]--->", {
  expect_equal(
    sum_run(x2, k = 5, lag = -3),
    runner(x2, k = 5, lag = -3, f = sum2))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_pad = TRUE),
    runner(x2, k = 5, lag = -3, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_rm = FALSE),
    runner(x2, k = 5, lag = -3, f = sum))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_pad = TRUE, na_rm = FALSE),
    runner(x2, k = 5, lag = -3, f = sum, na_pad = TRUE, type = "numeric"))



  expect_equal(
    sum_run(x2, k = 5, lag = -3)[ids],
    sum_run(x2, k = 5, lag = -3, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_pad = TRUE)[ids],
    sum_run(x2, k = 5, lag = -3, at = seq_along(x2)[ids], na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_rm = FALSE)[ids],
    sum_run(x2, k = 5, lag = -3, at = seq_along(x2)[ids], na_rm = FALSE))

  expect_equal(
    sum_run(x2, k = 5, lag = -3, na_pad = TRUE, na_rm = FALSE)[ids],
    sum_run(x2, k = 5, lag = -3, at = seq_along(x2)[ids], na_pad = TRUE, na_rm = FALSE))
})

test_that("       |--------+-[---]->", {
  expect_equal(
    sum_run(x2, k = 5, lag = -7),
    runner(x2, k = 5, lag = -7, f = sum2))

  expect_equal(
    sum_run(x2, k = 5, lag = -7, na_pad = TRUE),
    runner(x2, k = 5, lag = -7, f = sum2, na_pad = TRUE))



  expect_equal(
    sum_run(x2, k = 5, lag = -7)[ids],
    sum_run(x2, k = 5, lag = -7, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 5, lag = -7, na_pad = TRUE)[ids],
    sum_run(x2, k = 5, lag = -7, at = seq_along(x2)[ids], na_pad = TRUE))

})

test_that("       |--------+[]----->", {
  expect_equal(
    sum_run(x2, k = 1, lag = -1),
    runner(x2, k = 1, lag = -1, f = sum2))

  expect_equal(
    sum_run(x2, k = 1, lag = -1, na_pad = TRUE),
    runner(x2, k = 1, lag = -1, f = sum2, na_pad = TRUE))



  expect_equal(
    sum_run(x2, k = 1, lag = -1)[ids],
    sum_run(x2, k = 1, lag = -1, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 1, lag = -1, na_pad = TRUE)[ids],
    sum_run(x2, k = 1, lag = -1, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("       |------[]+------->", {
  expect_equal(
    sum_run(x2, k = 1, lag = 1),
    runner(x2, k = 1, lag = 1, f = sum2))

  expect_equal(
    sum_run(x2, k = 1, lag = 1, na_pad = TRUE),
    runner(x2, k = 1, lag = 1, f = sum2, na_pad = TRUE))



  expect_equal(
    sum_run(x2, k = 1, lag = 1)[ids],
    sum_run(x2, k = 1, lag = 1, at = seq_along(x2)[ids]))

  expect_equal(
    sum_run(x2, k = 1, lag = 1, na_pad = TRUE)[ids],
    sum_run(x2, k = 1, lag = 1, at = seq_along(x2)[ids], na_pad = TRUE))
})

test_that("various", {
  expect_equal(
    sum_run(x2, k = k, lag = 1),
    runner(x2, k = k, lag = 1, f = sum2))

  expect_equal(
    sum_run(x2, k = k, lag = 1, na_pad = TRUE),
    runner(x2, k = k, lag = 1, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, k = 3, lag = lag),
    runner(x2, k = 3, lag = lag, f = sum2))

  expect_equal(
    sum_run(x2, k = 3, lag = lag, na_pad = TRUE),
    runner(x2, k = 3, lag = lag, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = k, lag = lag),
    runner(x2, k = k, lag = lag, f = sum2))

  expect_equal(
    sum_run(x2, k = k, lag = lag, na_pad = TRUE),
    runner(x2, k = k, lag = lag, f = sum2, na_pad = TRUE))
})

test_that("date window", {
  expect_equal(
    sum_run(x2, lag = 3, idx = idx, na_pad = FALSE),
    runner(x2, lag = 3, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = 3, idx = idx, na_pad = TRUE),
    runner(x2, lag = 3, idx = idx, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, lag = -3, idx = idx, na_pad = FALSE),
    runner(x2, lag = -3, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = -3, idx = idx, na_pad = TRUE),
    runner(x2, lag = -3, idx = idx, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 3, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, k = 3, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, idx = idx, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, lag = -1, idx = idx, na_pad = FALSE),
    runner(x2, lag = -1, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = -1, idx = idx, na_pad = TRUE),
    runner(x2, lag = -1, idx = idx, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, lag = 100, idx = idx, na_pad = FALSE),
    runner(x2, lag = 100, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = 100, idx = idx, na_pad = TRUE),
    runner(x2, lag = 100, idx = idx, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, lag = -100, idx = idx, na_pad = FALSE),
    runner(x2, lag = -100, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = -100, idx = idx, na_pad = TRUE),
    runner(x2, lag = -100, idx = idx, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, lag = lag, idx = idx, na_pad = FALSE),
    runner(x2, lag = lag, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, lag = lag, idx = idx, na_pad = TRUE),
    runner(x2, lag = lag, idx = idx, f = sum2, na_pad = TRUE))

  expect_equal(
    sum_run(x2, k = 3, lag = 4, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, lag = 4, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, k = 3, lag = 4, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, lag = 4, idx = idx, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, k = 3, lag = -4, idx = idx, na_pad = FALSE),
    runner(x2, k = 3, lag = -4, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, k = 3, lag = -4, idx = idx, na_pad = TRUE),
    runner(x2, k = 3, lag = -4, idx = idx, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, k = k, lag = -4, idx = idx, na_pad = FALSE),
    runner(x2, k = k, lag = -4, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, k = k, lag = -4, idx = idx, na_pad = TRUE),
    runner(x2, k = k, lag = -4, idx = idx, f = sum2, na_pad = TRUE))


  expect_equal(
    sum_run(x2, k = 4, lag = lag, idx = idx, na_pad = FALSE),
    runner(x2, k = 4, lag = lag, idx = idx, f = sum2, na_pad = FALSE))

  expect_equal(
    sum_run(x2, k = 4, lag = lag, idx = idx, na_pad = TRUE),
    runner(x2, k = 4, lag = lag, idx = idx, f = sum2, na_pad = TRUE))
})
