context("set index by")

test_that("Test get_initial_call", {
  a <- function(x) {
    x
  }

  b <- function(arg) {
    c(arg)
  }

  c <- function(x) {
    get_initial_call(x)
  }

  data <- letters[1:2]

  expect_identical(
    a(data),
    letters[1:2]
  )

  expect_identical(
    a(x = "elo"),
    "elo"
  )

  init <- function(aa) {
    aa
  }

  expect_identical(
    a(x = init(letters[1:2])),
    letters[1:2]
  )
})

test_that("run_by", {
  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = "b")
  attr(x, "idx") <- "b"
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = b)
  attr(x, "idx") <- "b"
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = x$b)
  attr(x, "idx") <- x$b
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = letters[1:2])
  attr(x, "idx") <- letters[1:2]
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(
    x,
    k = letters[1:2],
    lag = letters[1:2],
    idx = letters[1:2],
    na_pad = letters[1:2],
    at = letters[1:2]
  )
  attr(x, "idx") <- letters[1:2]
  attr(x, "k") <- letters[1:2]
  attr(x, "lag") <- letters[1:2]
  attr(x, "idx") <- letters[1:2]
  attr(x, "na_pad") <- letters[1:2]
  attr(x, "at") <- letters[1:2]
  expect_identical(new_x, x)
})


test_that("set arg from attr", {
  data <- data.frame(
    x = 1:10,
    b = letters[1:10],
    kk = c(rep(2, 5), rep(5, 5))
  )

  x <- run_by(data, k = 1)
  expect_equal(attr(x, "k"), 1)

  x <- run_by(data, k = 1)
  expect_equal(attr(x, "k"), 1)

  x <- run_by(data, k = "k")
  expect_equal(attr(x, "k"), "k")

  x <- run_by(data, k = k)
  expect_equal(attr(x, "k"), "k")
})

test_that("run_by %>% runner", {
  data <- data.frame(
    index = cumsum(sample(0:3, 10, replace = TRUE)),
    x = 1:10,
    b = letters[1:10],
    k = c(rep(2, 5), rep(5, 5)),
    lag = sample(0:3, 10, replace = TRUE)
  )

  x <- run_by(data, idx = index, k = 2)
  expect_identical(
    runner(x, f = function(x) mean(x$x)),
    runner(data, f = function(x) mean(x$x), idx = data$index, k = 2)
  )


  x <- run_by(data, idx = index, k = 5, lag = 2, na_pad = FALSE, at = 10)

  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = data$index,
      k = 5,
      lag = 2,
      na_pad = FALSE,
      at = 10
    )
  )


  x <- run_by(data, idx = index, k = 5, lag = 2, na_pad = FALSE, at = 1:20)
  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = data$index,
      k = 5,
      lag = 2,
      na_pad = FALSE,
      at = 1:20
    )
  )

  x <- run_by(data, idx = index, k = k, lag = "lag", na_pad = FALSE)
  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = data$index,
      k = data$k,
      lag = data$lag,
      na_pad = FALSE
    )
  )
})
