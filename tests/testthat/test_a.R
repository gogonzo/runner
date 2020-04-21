context("a")

test_that("", {
  data <- data.frame(
    index = cumsum(sample(0:3, 10, replace = TRUE)),
    x = 1:10,
    b = letters[1:10],
    k = c(rep(2, 5), rep(5, 5)),
    lag = sample(0:3, 10, replace = TRUE)
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
      idx = index,
      k = data$k,
      lag = data$lag,
      na_pad = FALSE
    )
  )



})
