context("Test Runner")
x1 <- rnorm(100)
x2 <- sample(c(rep(NA, 5), rnorm(15)), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
find_idx <- function(x, i, k, lag = 0, na_pad = FALSE) {
  n <- length(x)
  if (missing(k)) {
    if ((i - lag) > n) if (na_pad) return(integer(0))
    seq_along(x) %in% seq(0, i - lag)
  } else {
    if ((i - k - lag + 1) < 1 || (i - lag) > n) if (na_pad) return(integer(0))
    seq_along(x) %in% seq(i - lag - k + 1, i - lag)
  }
}

test_that("       |--------]------->", {
  expect_identical(
    runner(x1, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i)])))
})

test_that("   [...|----]---+------->", {
  expect_equal(
    runner(x1, lag = 3, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = 3)])))

  expect_equal(
    runner(x1, lag = 3, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = 3)])))
})

test_that("       |--------+---]--->", {
  expect_equal(
    runner(x1, lag = -3, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = -3)])))

  expect_equal(
    runner(x1, lag = -3, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = -3, na_pad = TRUE)])))

  expect_equal(
    runner(x1, lag = lag, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = lag[i])])))

  expect_equal(
    runner(x1, lag = lag, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = lag[i], na_pad = TRUE)])))
})

test_that("  [...]|--------+------->", {
  expect_equal(
    runner(x1, lag = 100, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = 100)])))

  expect_equal(
    runner(x1, lag = -100, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = -100)])))

  expect_equal(
    runner(x1, lag = 100, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = 100, na_pad = TRUE)])))

  expect_equal(
    runner(x1, lag = -100, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, lag = -100, na_pad = TRUE)])))
})

test_that("       |----[...]------->", {
  expect_equal(
    runner(x1, k = 3, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 3)])))

  expect_equal(
    runner(x1, k = 3, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 3, na_pad = TRUE)])))
})

test_that("       [...|--------+-------[...]", {
  expect_equal(
    runner(x1, k = 100, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 100)])))

  expect_equal(
    runner(x1, k = 100, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 100, na_pad = TRUE)])))

  expect_equal(
    runner(x1, k = 101, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101)])))

  expect_equal(
    runner(x1, k = 101, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 101, na_pad = TRUE)])))
})

test_that("       [...|----]---+------->", {
  expect_equal(
    runner(x1, k = 5, lag = 3, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = 3)])))

  expect_equal(
    runner(x1, k = 5, lag = 3, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = 3, na_pad = TRUE)])))
})

test_that("       |-----[--+---]--->", {
  expect_equal(
    runner(x1, k = 5, lag = -3, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = -3)])))

  expect_equal(
    runner(x1, k = 5, lag = -3, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = -3, na_pad = TRUE)])))
})

test_that("       |--------+-[---]->", {
  expect_equal(
    runner(x1, k = 5, lag = -7, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = -7)])))

  expect_equal(
    runner(x1, k = 5, lag = -7, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 5, lag = -7, na_pad = TRUE)])))
})

test_that("       |--------+[]----->", {
  expect_equal(
    runner(x1, k = 1, lag = -1, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 1, lag = -1)])))

  expect_equal(
    runner(x1, k = 1, lag = -1, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 1, lag = -1, na_pad = TRUE)])))
})

test_that("       |------[]+------->", {
  expect_equal(
    runner(x1, k = 1, lag = 1, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 1, lag = 1)])))

  expect_equal(
    runner(x1, k = 1, lag = 1, f = mean, na_pad = TRUE),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 1, lag = 1, na_pad = TRUE)])))
})

test_that("various", {
  expect_equal(
    runner(x1, k = k, lag = 1, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = k[i], lag = 1)])))

  expect_equal(
    runner(x1, k = 3, lag = lag, f = mean),
    sapply(seq_along(x1), function(i) mean(x1[find_idx(x1, i = i, k = 3, lag = lag[i])])))

  expect_equal(
    runner(x1, k = k, lag = lag, f = mean),
    sapply(window_run(x1, k = k, lag = lag), mean))
})

test_that("date window", {
  expect_equal(
    runner(x1, lag = 3, idx = idx, f = mean),
    sapply(window_run(x1, lag = 3, idx = idx), mean)
  )

  expect_equal(
    runner(x2, lag = 3, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, lag = 3, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, lag = -3, idx = idx, f = mean),
    sapply(window_run(x1, lag = -3, idx = idx), mean)
  )

  expect_equal(
    runner(x2, lag = -3, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, lag = -3, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, lag = -1, idx = idx, f = mean),
    sapply(window_run(x1, lag = -1, idx = idx), mean)
  )

  expect_equal(
    runner(x2, lag = -1, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, lag = -1, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, lag = -100, idx = idx, f = mean),
    sapply(window_run(x1, lag = -100, idx = idx), mean)
  )

  expect_equal(
    runner(x2, lag = -100, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, lag = -100, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, lag = lag, idx = idx, f = mean),
    sapply(window_run(x1, lag = lag, idx = idx), mean)
  )

  expect_equal(
    runner(x2, lag = -lag, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, lag = -lag, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, k = 3, lag = 3, idx = idx, f = mean),
    sapply(window_run(x1, k = 3, lag = 3, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = 3, lag = 3, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, k = 3, lag = 3, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, k = 3, lag = -3, idx = idx, f = mean),
    sapply(window_run(x1, k = 3, lag = -3, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = 3, lag = -3, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, k = 3, lag = -3, idx = idx, na_pad = TRUE), mean)
  )


  expect_equal(
    runner(x1, k = 3, lag = lag, idx = idx, f = mean),
    sapply(window_run(x1, k = 3, lag = lag, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = 3, lag = lag, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, k = 3, lag = lag, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, k = k, lag = lag, idx = idx, f = mean),
    sapply(window_run(x1, k = k, lag = lag, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = k, lag = lag, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, k = k, lag = lag, idx = idx, na_pad = TRUE), mean)
  )

  expect_equal(
    runner(x1, k = 3, lag = lag, idx = idx, f = mean),
    sapply(window_run(x1, k = 3, lag = lag, idx = idx), mean)
  )

  expect_equal(
    runner(x2, k = k, lag = 3, idx = idx, f = mean, na_pad = TRUE),
    sapply(window_run(x2, k = k, lag = 3, idx = idx, na_pad = TRUE), mean)
  )
})

test_that("Function applied on other types", {
    expect_silent(runner(as.integer(1:100), k = 5, f = length))
    expect_silent(runner(as.integer(1:100), k = k, f = length))
    expect_silent(runner(as.integer(1:100), k = k, idx, f = length))

    expect_silent(runner(sample(letters, 100, replace = TRUE), k = 5, f = length))
    expect_silent(runner(sample(letters, 100, replace = TRUE), k = k, f = length))
    expect_silent(runner(sample(letters, 100, replace = TRUE), k = k, idx, f = length))

    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = 5, f = length))
    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = k, f = length))
    expect_silent(runner(as.factor(sample(letters, 100, replace = TRUE)), k = k, idx, f = length))

    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = 5, f = length))
    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = k, f = length))
    expect_silent(runner(as.Date(1:100, origin = "1970-01-01"), k = k, idx, f = length))
})

test_that("Errors", {
  expect_error(runner(x = letters[1:5]))
  expect_error(runner(x = letters[1:5], f = ""))

  expect_error(runner(list(1:10), k = 5, f = mean), "Invalid data type")

  expect_error(runner(1:10, k = (1:9), f = mean), "length of k and length of x differs")
  expect_error(runner(1:10, k = c(NA, 1:9), f = mean), "Function doesn't accept NA values in k vector")

  expect_error(runner(1:10, lag = (1:9), f = mean), "length of lag and length of x differs")
  expect_error(runner(1:10, lag = c(NA, 1:9), f = mean), "Function doesn't accept NA values in lag vector")

  expect_error(runner(1:10, idx = (1:9), f = mean), "length of idx and length of x differs")
  expect_error(runner(1:10, idx = c(NA, 1:9), f = mean), "Function doesn't accept NA values in idx vector")
})
