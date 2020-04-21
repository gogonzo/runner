context("set index by")

# Test get_initial_call ------
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

# run_by ------
test_that("run_by", {
  x <- data.frame(x = 1:2, b = letters[1:2])
  expect_error(
    run_by(x, idx = "b"),
    "idx should not be character"
  )
  expect_error(
    run_by(x, idx = "1 day"),
    "idx should not be character"
  )

  expect_error(
    run_by(x, k = "-1 day"),
    "k should not be character unless"
  )

  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = b)
  attr(x, "idx") <- as.name("b")
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = x$b)
  attr(x, "idx") <- x$b
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(x, idx = 1:2)
  attr(x, "idx") <- 1:2
  expect_identical(new_x, x)


  x <- data.frame(x = 1:2, b = letters[1:2])
  new_x <- run_by(
    x,
    k = 1:2,
    lag = 1:2,
    idx = 1:2,
    na_pad = 1:2,
    at = 1:2
  )
  attr(x, "idx") <- 1:2
  attr(x, "k") <- 1:2
  attr(x, "lag") <- 1:2
  attr(x, "idx") <- 1:2
  attr(x, "na_pad") <- 1:2
  attr(x, "at") <- 1:2
  expect_identical(new_x, x)
})

# set arg from attr --------
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

  expect_error(
    run_by(data, k = "k"),
    "k should not be character unless"
  )

  expect_error(
    run_by(data, k = k),
    "object 'k' not found"
  )
  k <- 1

  new_x <- run_by(data, k = k)
  attr(x, "k") <- k
  expect_identical(new_x, x)

  new_x <- run_by(data, k = kk)
  expect_equal(attr(new_x, "k"), as.name("kk"))

  expect_error(
    run_by(1:10, idx = 1:10),
    "`run_by` should be used only for data.frame."
  )
})

# run_by %>% runner -----
test_that("run_by %>% runner", {
  data <- data.frame(
    index = cumsum(sample(0:3, 10, replace = TRUE)),
    x = 1:10,
    b = letters[1:10],
    k = c(rep(2, 5), rep(5, 5)),
    lag = sample(0:3, 10, replace = TRUE)
  )

  # args by name
  x <- run_by(data, idx = index, k = k)
  expect_identical(
    runner(x, f = function(x) mean(x$x)),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = data$index,
      k = k
    )
  )


  # all
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


  # argument by explicit value
  x <- run_by(data, idx = index, k = 5, lag = 2, na_pad = FALSE, at = 1:20)
  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = index,
      k = 5,
      lag = 2,
      na_pad = FALSE,
      at = 1:20
    )
  )

  # few args by name / runner misc
  x <- run_by(data, idx = index, k = k, lag = lag, na_pad = FALSE)
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
      lag = lag,
      na_pad = FALSE
    )
  )

  # arg by name, while obj of the same name in global env (by column name should be first)
  index <- 1:10
  x <- run_by(data, idx = index, k = k, lag = lag, na_pad = FALSE)
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

  x <- run_by(data, idx = data$index, k = k, lag = lag, na_pad = FALSE)
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

  x <- run_by(data, idx = data$index, k = k, lag = lag, na_pad = FALSE)
  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = index,
      k = data$k,
      lag = data$lag,
      na_pad = FALSE
    )
  )

  # by object from global env
  index2 <- 1:10
  x <- run_by(data, idx = index2, k = k, lag = lag, na_pad = FALSE)
  expect_identical(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    runner(
      data,
      f = function(x) mean(x$x),
      idx = index2,
      k = data$k,
      lag = data$lag,
      na_pad = FALSE
    )
  )
})


test_that("run_by %>% runner errors", {
  data <- data.frame(
    index = cumsum(sample(0:3, 10, replace = TRUE)),
    x = 1:10,
    b = letters[1:10],
    k = c(rep(2, 5), rep(5, 5)),
    lag = sample(0:3, 10, replace = TRUE)
  )

  x <- run_by(data, idx = index, k = k, lag = lag, na_pad = FALSE)
  names(x)[1] <- "index2"
  expect_error(
    runner(
      x,
      f = function(x) mean(x$x)
    ),
    "does not exist in x"
  )

  x <- run_by(data, idx = index)
  expect_warning(
    runner(
      x,
      idx = 1:10,
      f = function(x) mean(x$x)
    ),
    "idx set in run_by"
  )


  expect_error(
    runner(
      data,
      idx = "index",
      f = function(x) mean(x$x)
    ),
    "idx should not be character unless "
  )

  expect_error(
    runner(
      data,
      k = "-1 day",
      f = function(x) mean(x$x)
    ),
    "k should not be character unless you specify"
  )
})
