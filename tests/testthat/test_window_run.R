context("Running window")
set.seed(11)
x1 <- 1:100
x2 <- as.character(1:100)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
find_idx <- function(x, i, k, lag = 0, na_pad = FALSE) {
  n <- length(x)
  if (missing(k)) {
    if ((i - lag) > n) if (na_pad) return(x[0])
    seq_along(x) %in% seq(0, i - lag)
  } else {
    if ((i - k - lag + 1) < 1 || (i - lag) > n) if (na_pad) return(x[0])
    seq_along(x) %in% seq(i - lag - k + 1, i - lag)
  }
}
find_idx_date <- function(x, i, k, lag = 0, idx, na_pad = FALSE) {
  n <- length(x)
  if (missing(k)) {
    if ((idx[i] - lag) > idx[n]) if (na_pad) return(x[0])
    x[idx <= (idx[i] - lag)]
  } else {
    if ((idx[i] - k - lag + 1) < idx[1] || (idx[i] - lag) > idx[n]) if (na_pad) return(x[0])
    x[idx %in% seq(idx[i] - lag - k + 1, idx[i] - lag)]
  }

}
at <- sample(1:100, 10)
at_date <- sample(idx, 10)
ids <- match(at_date, idx)

test_that("       |--------]------->", {
  expect_identical(
    window_run(x1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i)]))

  expect_identical(
    window_run(x1, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i)])[at])
})

test_that("   [...|----]---+------->", {
  expect_identical(
    window_run(x1, lag = 3),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 3)]))

  expect_identical(
    window_run(x1, lag = 3, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 3, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, lag = 3, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 3)])[at])

  expect_identical(
    window_run(x1, lag = 3, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 3, na_pad = TRUE)])[at])
})

test_that("       |--------+---]--->", {
  expect_identical(
    window_run(x1, lag = -3),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -3)]))

  expect_identical(
    window_run(x1, lag = -3, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -3, na_pad = TRUE)]))

  expect_identical(
    window_run(x1, lag = lag),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = lag[i])]))

  expect_identical(
    window_run(x1, lag = lag, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = lag[i], na_pad = TRUE)]))




  expect_identical(
    window_run(x1, lag = -3, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -3)])[at])

  expect_identical(
    window_run(x1, lag = -3, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -3, na_pad = TRUE)])[at])

  expect_identical(
    window_run(x1, lag = lag[at], at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = lag[i])])[at])

  expect_identical(
    window_run(x1, lag = lag[at], na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = lag[i], na_pad = TRUE)])[at])
})

test_that("  [...]|--------+------->", {
  expect_identical(
    window_run(x1, lag = 100),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 100)]))

  expect_identical(
    window_run(x1, lag = 100, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 100, na_pad = TRUE)]))


  expect_identical(
    window_run(x1, lag = -100),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -100)]))

  expect_identical(
    window_run(x1, lag = -100, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -100, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, lag = 100, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 100)])[at])

  expect_identical(
    window_run(x1, lag = 100, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = 100, na_pad = TRUE)])[at])

  expect_identical(
    window_run(x1, lag = -100, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -100)])[at])

  expect_identical(
    window_run(x1, lag = -100, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, lag = -100, na_pad = TRUE)])[at])
})

test_that("       |----[...]------->", {
  expect_identical(
    window_run(x1, k = 3),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3)]))

  expect_identical(
    window_run(x1, k = 3, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 3, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3)])[at])

  expect_identical(
    window_run(x1, k = 3, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, na_pad = TRUE)])[at])
})

test_that("       [...|--------+-------[...]", {
  expect_identical(
    window_run(x1, k = 1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1)]))

  expect_identical(
    window_run(x1, k = 1, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, na_pad = TRUE)]))

  expect_identical(
    window_run(x1, k = 99),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 99)]))

  expect_identical(
    window_run(x1, k = 99, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 99, na_pad = TRUE)]))

  expect_identical(
    window_run(x1, k = 100),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 100)]))

  expect_identical(
    window_run(x1, k = 100, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 100, na_pad = TRUE)]))

  expect_equal(
    runner(x1, k = 101, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101)])))

  expect_equal(
    runner(x1, k = 101, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101, na_pad = TRUE)])))



  expect_identical(
    window_run(x1, k = 1, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1)])[at])

  expect_identical(
    window_run(x1, k = 1, na_pad = TRUE, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, na_pad = TRUE)])[at])

  expect_identical(
    window_run(x1, k = 99, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 99)])[at])

  expect_identical(
    window_run(x1, k = 99, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 99, na_pad = TRUE)])[at])

  expect_identical(
    window_run(x1, k = 100, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 100)])[at])

  expect_identical(
    window_run(x1, k = 100, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 100, na_pad = TRUE)])[at])

  expect_equal(
    runner(x1, k = 101, at = at, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101)]))[at])

  expect_equal(
    runner(x1, k = 101, at = at, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101, na_pad = TRUE)]))[at])
})

test_that("       [...|----]---+------->", {
  expect_identical(
    window_run(x1, k = 5, lag = 3),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = 3)]))

  expect_identical(
    window_run(x1, k = 5, lag = 3, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = 3, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 5, lag = 3, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = 3)])[at])

  expect_identical(
    window_run(x1, k = 5, lag = 3, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = 3, na_pad = TRUE)])[at])
})

test_that("       |-----[--+---]--->", {
  expect_identical(
    window_run(x1, k = 5, lag = -3),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -3)]))

  expect_identical(
    window_run(x1, k = 5, lag = -3, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -3, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 5, lag = -3, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -3)])[at])

  expect_identical(
    window_run(x1, k = 5, lag = -3, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -3, na_pad = TRUE)])[at])
})

test_that("       |--------+-[---]->", {
  expect_identical(
    window_run(x1, k = 5, lag = -7),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -7)]))

  expect_identical(
    window_run(x1, k = 5, lag = -7, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -7, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 5, lag = -7, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -7)])[at])

  expect_identical(
    window_run(x1, k = 5, lag = -7, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 5, lag = -7, na_pad = TRUE)])[at])
})

test_that("       |--------+[]----->", {
  expect_identical(
    window_run(x1, k = 1, lag = -1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = -1)]))

  expect_identical(
    window_run(x1, k = 1, lag = -1, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = -1, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 1, lag = -1, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = -1)])[at])

  expect_identical(
    window_run(x1, k = 1, lag = -1, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = -1, na_pad = TRUE)])[at])
})

test_that("       |------[]+------->", {
  expect_identical(
    window_run(x1, k = 1, lag = 1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = 1)]))

  expect_identical(
    window_run(x1, k = 1, lag = 1, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = 1, na_pad = TRUE)]))



  expect_identical(
    window_run(x1, k = 1, lag = 1, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = 1)])[at])

  expect_identical(
    window_run(x1, k = 1, lag = 1, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 1, lag = 1, na_pad = TRUE)])[at])
})

test_that("various", {
  expect_identical(
    window_run(x1, k = k, lag = 1),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 1)]))

  expect_identical(
    window_run(x1, k = k, lag = 1, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 1, na_pad = TRUE)]))

  expect_equal(
    window_run(x1, k = 3, lag = lag),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, lag = lag[i])]))

  expect_equal(
    window_run(x1, k = 3, lag = lag, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, lag = lag[i], na_pad = TRUE)]))

  expect_equal(
    window_run(x1, k = k, lag = lag),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = lag[i])]))

  expect_equal(
    window_run(x1, k = k, lag = lag, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = lag[i], na_pad = TRUE)]))

  expect_equal(
    window_run(x1, k = k, lag = 4),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 4)]))

  expect_equal(
    window_run(x1, k = k, lag = 4, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 4, na_pad = TRUE)]))





  expect_identical(
    window_run(x1, k = k[at], lag = 1, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 1)])[at])

  expect_identical(
    window_run(x1, k = k[at], lag = 1, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 1, na_pad = TRUE)])[at])

  expect_equal(
    window_run(x1, k = 3, lag = lag[at], at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, lag = lag[i])])[at])

  expect_equal(
    window_run(x1, k = 3, lag = lag[at], at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = 3, lag = lag[i], na_pad = TRUE)])[at])

  expect_equal(
    window_run(x1, k = k[at], lag = lag[at], at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = lag[i])])[at])

  expect_equal(
    window_run(x1, k = k[at], lag = lag[at], at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = lag[i], na_pad = TRUE)])[at])

  expect_equal(
    window_run(x1, k = k[at], lag = 4, at = at),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 4)])[at])

  expect_equal(
    window_run(x1, k = k[at], lag = 4, at = at, na_pad = TRUE),
    lapply(seq_along(x1), function(i) x1[find_idx(x1, i = i, k = k[i], lag = 4, na_pad = TRUE)])[at])

})

test_that("window_run with idx same as window_run with windows",{
  expect_identical(window_run(x1, k = 3) ,
                   window_run(x1, k = 3, idx = 1:100))

  expect_identical(window_run(x1, k = k) ,
                   window_run(x1, k = k, idx = 1:100))

  expect_identical(window_run(x1, k = k, lag = 5),
                   window_run(x1, k = k, lag = 5, idx = 1:100))

  expect_identical(window_run(x1, k = k, lag = lag),
                   window_run(x1, k = k, lag = lag, idx = 1:100))

  expect_identical(window_run(x1, k = 3, na_pad = TRUE) ,
                   window_run(x1, k = 3, idx = 1:100, na_pad = TRUE))

  expect_identical(window_run(x1, k = k, na_pad = TRUE) ,
                   window_run(x1, k = k, idx = 1:100, na_pad = TRUE))

  expect_identical(window_run(x1, k = k, lag = 5, na_pad = TRUE),
                   window_run(x1, k = k, lag = 5, idx = 1:100, na_pad = TRUE))

  expect_identical(window_run(x1, k = k, lag = lag, na_pad = TRUE),
                   window_run(x1, k = k, lag = lag, idx = 1:100, na_pad = TRUE))




  expect_identical(window_run(x1, k = 3, at = at) ,
                   window_run(x1, k = 3, idx = 1:100, at = at))

  expect_identical(window_run(x1, k = k[at], at = at) ,
                   window_run(x1, k = k[at], idx = 1:100, at = at))

  expect_identical(window_run(x1, k = k[at], lag = 5, at = at),
                   window_run(x1, k = k[at], lag = 5, idx = 1:100, at = at))

  expect_identical(window_run(x1, k = k[at], lag = lag[at], at = at),
                   window_run(x1, k = k[at], lag = lag[at], idx = 1:100, at = at))

  expect_identical(window_run(x1, k = 3, na_pad = TRUE, at = at) ,
                   window_run(x1, k = 3, idx = 1:100, na_pad = TRUE, at = at))

  expect_identical(window_run(x1, k = k[at], na_pad = TRUE, at = at) ,
                   window_run(x1, k = k[at], idx = 1:100, na_pad = TRUE, at = at))

  expect_identical(window_run(x1, k = k[at], lag = 5, na_pad = TRUE, at = at),
                   window_run(x1, k = k[at], lag = 5, idx = 1:100, na_pad = TRUE, at = at))

  expect_identical(window_run(x1, k = k[at], lag = lag[at], na_pad = TRUE, at = at),
                   window_run(x1, k = k[at], lag = lag[at], idx = 1:100, na_pad = TRUE, at = at))

})

test_that("date - only lag", {


  out <- window_run(x2, lag = 3, idx = idx,na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = 3, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, lag = 3, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = 3, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, lag = -3, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, lag = -3, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, lag = lag, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = lag[i], idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, lag = lag, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = lag[i], idx = idx, na_pad = TRUE))
  expect_identical(out, test)



  out <- window_run(x2, lag = 3, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = 3, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, lag = 3, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = 3, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, lag = -3, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, lag = -3, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, lag = lag[ids], idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = lag[i], idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, lag = lag[ids], idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, lag = lag[i], idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])
})

test_that("date - lag + k", {
  out <- window_run(x2, k = 3, lag = 3, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = 3, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = 3, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = 3, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -3, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -3, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = 4, lag = -3, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 4, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 4, lag = -3, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 4, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -4, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -4, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -4, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -4, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -1, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -1, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 3, lag = -1, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -1, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = 1, lag = -1, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 1, lag = -1, idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = 1, lag = -1, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 1, lag = -1, idx = idx, na_pad = TRUE))
  expect_identical(out, test)

  out <- window_run(x2, k = k, lag = lag, idx = idx, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = k[i], lag = lag[i], idx = idx, na_pad = FALSE))
  expect_identical(out, test)

  out <- window_run(x2, k = k, lag = lag, idx = idx, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = k[i], lag = lag[i], idx = idx, na_pad = TRUE))
  expect_identical(out, test)






  out <- window_run(x2, k = 3, lag = 3, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = 3, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = 3, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = 3, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -3, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -3, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 4, lag = -3, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 4, lag = -3, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 4, lag = -3, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 4, lag = -3, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -4, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -4, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -4, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -4, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -1, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -1, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 3, lag = -1, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 3, lag = -1, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 1, lag = -1, idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 1, lag = -1, idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = 1, lag = -1, idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = 1, lag = -1, idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = k[ids], lag = lag[ids], idx = idx, at = at_date, na_pad = FALSE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = k[i], lag = lag[i], idx = idx, na_pad = FALSE))
  expect_identical(out, test[ids])

  out <- window_run(x2, k = k[ids], lag = lag[ids], idx = idx, at = at_date, na_pad = TRUE)
  test <- lapply(seq_along(x2), function(i) find_idx_date(x2, i = i, k = k[i], lag = lag[i], idx = idx, na_pad = TRUE))
  expect_identical(out, test[ids])
})

test_that("Test non-numeric arguments", {
  expect_silent(window_run(as.integer(1:10), k = 5))
  expect_silent(window_run(letters[1:10], k = 5))
  expect_silent(window_run(as.factor(letters[1:10]), k = 5))
  expect_silent(window_run(seq(Sys.Date(), Sys.Date() + 9, by = "1 day"), k = 5))
  expect_silent(window_run(sample(c(TRUE, FALSE), 10, replace = TRUE), k = 5))
})

test_that("Errors", {
  expect_error(window_run(list(1:10), k = 5), "Invalid \\'x\\' type")

  expect_error(window_run(1:10, k = (1:9)), "length of k and length of x differs")
  expect_error(window_run(1:10, k = c(NA, 1:9)), "Function doesn't accept NA values in k vector")

  expect_error(window_run(1:10, lag = (1:9)), "length of lag and length of x differs")
  expect_error(window_run(1:10, lag = c(NA, 1:9)), "Function doesn't accept NA values in lag vector")

  expect_error(window_run(1:10, idx = (1:9)), "length of idx and length of x differs")
  expect_error(window_run(1:10, idx = c(NA, 1:9)), "Function doesn't accept NA values in idx vector")
  })
