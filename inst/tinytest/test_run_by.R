# run_by ------
x <- data.frame(x = 1:2, b = letters[1:2])

expect_error(
  run_by(x, idx = b),
  "object 'b' not found"
)
expect_error(
  run_by(x, idx = "1 day"),
  "`idx` should be either:"
)

expect_error(
  run_by(x, idx = c("x", "b")),
  "`idx` should be either:"
)

expect_error(
  run_by(x, idx = difftime(Sys.Date() - 2, Sys.Date())),
  "`idx` should be either:"
)

expect_error(
  run_by(x, k = "-1 day"),
  "`k` is invalid, should be either"
)


x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = "b")
attr(x, "idx") <- "b"
expect_identical(new_x, x)


dates <- c(Sys.Date(), Sys.Date())
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = dates)
attr(x, "idx") <- dates
expect_identical(new_x, x)


time <- c(Sys.time(), Sys.time())
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = time)
attr(x, "idx") <- time
expect_identical(new_x, x)


x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = "x")
attr(x, "k") <- "x"
expect_identical(new_x, x)

difftime <- c(as.difftime(1, units = "secs"), as.difftime(2, units = "secs"))
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x)


difftime <- as.difftime(1, units = "secs")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x)


difftime <- c("-1 secs", "2 weeks")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x)


difftime <- c("-1 secs")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x)


x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = x$x)
attr(x, "idx") <- x$x
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
  na_pad = TRUE,
  at = 1:2
)

attr(x, "k") <- 1:2
attr(x, "lag") <- 1:2
attr(x, "idx") <- 1:2
attr(x, "na_pad") <- TRUE
attr(x, "at") <- 1:2
expect_identical(new_x, x)

# set arg from attr ------
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
  run_by(data, k = k),
  "object 'k' not found"
)
k <- 1

new_x <- run_by(data, k = k)
attr(x, "k") <- k
expect_identical(new_x, x)


expect_error(
  run_by(1:10, idx = 1:10),
  "`run_by` should be used only for `data.frame`."
)

# run_by %>% runner -----
data <- data.frame(
  index = cumsum(sample(0:3, 10, replace = TRUE)),
  x = 1:10,
  b = letters[1:10],
  k = c(rep(2, 5), rep(5, 5)),
  lag = sample(0:3, 10, replace = TRUE)
)

# args by name
x <- run_by(data, idx = "index", k = "k")
expect_identical(
  runner(x, f = function(x) mean(x$x)),
  runner(
    data,
    f = function(x) mean(x$x),
    idx = data$index,
    k = "k"
  )
)

# args by name
x <- run_by(data, k = "k")
expect_warning(
  expect_error(
    runner(x, f = function(x) mean(x$x), k = "1 day"),
    "`k` is invalid, should be either"
  ),
  "`k` set in run_by"
)

x <- run_by(data, at = "index", idx = "index")
expect_identical(
  runner(x, f = function(x) mean(x$x)),
  runner(data, f = function(x) mean(x$x), at = "index", idx = "index")
)

x <- run_by(data, at = "index", idx = "index")
expect_warning(
  runner(x, f = function(x) mean(x$x), at = 1:3),
  "`at` set in run_by"
)

x <- run_by(data, at = 1:3, idx = "index")
expect_warning(
  runner(x, f = function(x) mean(x$x), at = "index"),
  "`at` set in run_by"
)

x <- run_by(data, at = 1:3, idx = "index")
expect_warning(
  expect_error(
    runner(x, f = function(x) mean(x$x), at = "index2"),
    "`at` should be either"
  ),
  "`at` set in run_by"
)


x <- run_by(data, at = "index")
names(x)[1] <- "wrong"
expect_error(
  runner(x, f = function(x) mean(x$x)),
  "`at` should be either"
)


# all
x <- run_by(data, idx = "index", k = 5, lag = 2, na_pad = FALSE, at = 10)
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
x <- run_by(data, idx = "index", k = 5, lag = 2, na_pad = FALSE, at = 1:20)
expect_identical(
  runner(
    x,
    f = function(x) mean(x$x)
  ),
  runner(
    data,
    f = function(x) mean(x$x),
    idx = "index",
    k = 5,
    lag = 2,
    na_pad = FALSE,
    at = 1:20
  )
)

# few args by name / runner misc
x <- run_by(data, idx = "index", k = "k", lag = "lag", na_pad = FALSE)
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
    lag = "lag",
    na_pad = FALSE
  )
)

# arg by name, while obj of the same name
# in global env (by column name should be first)
index <- 1:10
x <- run_by(data, idx = "index", k = "k", lag = "lag", na_pad = FALSE)
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

x <- run_by(data, idx = data$index, k = "k", lag = "lag", na_pad = FALSE)
expect_identical(
  runner(
    x,
    f = function(x) mean(x$x)
  ),
  runner(
    data,
    f = function(x) mean(x$x),
    idx = "index",
    k = data$k,
    lag = data$lag,
    na_pad = FALSE
  )
)

# run_by %>% runner errors -------
data <- data.frame(
  index = cumsum(sample(0:3, 10, replace = TRUE)),
  x = 1:10,
  b = letters[1:10],
  k = c(rep(2, 5), rep(5, 5)),
  lag = sample(0:3, 10, replace = TRUE)
)

x <- run_by(data, idx = "index")
names(x)[1] <- "index2"
expect_error(
  runner(
    x,
    f = function(x) mean(x$x)
  ),
  "`idx` should be either:"
)

x <- run_by(data, idx = "index")
expect_warning(
  expect_error(
    runner(
      x,
      idx = "index2",
      f = function(x) mean(x$x)
    ),
    "`idx` should be either:"
  ),
  "`idx` set in run_by"
)


x <- run_by(data, at = "index")
expect_warning(
  expect_error(
    runner(
      x,
      at = "1 day",
      f = function(x) mean(x$x)
    ),
    "`at` should be either:"
  ),
  "`at` set in run_by"
)

expect_error(
  run_by(data, k = as.factor(1:10)),
  "`k` is invalid, should be either:"
)

expect_error(
  run_by(data, idx = as.factor(1:10)),
  "`idx` should be either:"
)

expect_error(
  run_by(data, at = as.factor(1:10)),
  "`at` should be either:"
)

expect_error(
  run_by(data, lag = "1 day"),
  "`lag` is invalid, should be either"
)



x <- run_by(
  data,
  idx = "index",
  k = as.difftime(1, units = "secs"),
  na_pad = TRUE
)
expect_warning(
  runner(
    x,
    idx = 1:10,
    f = function(x) mean(x$x)
  ),
  "`idx` set in run_by"
)

expect_warning(
  runner(
    x,
    k = 1:10,
    f = function(x) mean(x$x)
  ),
  "`k` set in run_by"
)

expect_warning(
  runner(
    x,
    na_pad = FALSE,
    f = function(x) mean(x$x)
  ),
  "`na_pad` set in run_by"
)

expect_error(
  runner(
    data,
    k = "-1 day",
    f = function(x) mean(x$x)
  ),
  "`k` is invalid, should be either"
)
