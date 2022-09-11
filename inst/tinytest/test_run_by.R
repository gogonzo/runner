# run_by ------
x <- data.frame(x = 1:2, b = letters[1:2])

expect_error(run_by(x, idx = b), "object 'b' not found")
expect_error(run_by(x, idx = "1 day"), "`idx` is invalid")
expect_error(run_by(x, idx = c("x", "b")), "`idx` is invalid")
expect_error(run_by(x, idx = difftime(Sys.Date() - 2, Sys.Date())), "`idx` is invalid")
expect_error(run_by(x, k = "-1 day"), "`k` is invalid")

x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = "x")
attr(x, "idx") <- "x"
expect_identical(new_x, x, info = "run_by keeps `idx` as column name")

dates <- c(Sys.Date(), Sys.Date())
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = dates)
attr(x, "idx") <- dates
expect_identical(new_x, x, info = "run_by can hold a passed date vector")


time <- c(Sys.time(), Sys.time())
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, idx = time)
attr(x, "idx") <- time
expect_identical(new_x, x, info = "run_by can hold a passed datetime vector")


x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = "x")
attr(x, "k") <- "x"
expect_identical(new_x, x, info = "run_by keeps `k` as column name")

difftime <- c(as.difftime(1, units = "secs"), as.difftime(2, units = "secs"))
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x, info = "run_by keeps difftime vector as k attribute")


difftime <- as.difftime(1, units = "secs")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x, info = "run_by keeps difftime vector as k attribute")

difftime <- c("-1 secs", "2 weeks")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x, info = "run_by keeps difftime vector as k attribute")


difftime <- c("-1 secs")
x <- data.frame(x = 1:2, b = letters[1:2])
new_x <- run_by(x, k = difftime)
attr(x, "k") <- difftime
expect_identical(new_x, x, info = "run_by keeps difftime vector as k attribute")


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

k <- 1
new_x <- run_by(data, k = k)
attr(x, "k") <- k
expect_identical(new_x, x)


expect_error(
  run_by(1:10, idx = 1:10),
  "`run_by` should be used only with `data.frame`."
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
expect_error(
  runner(x, f = function(x) mean(x$x), k = "1 day"),
  "`k` is invalid, should be either"
)

x <- run_by(data, at = "index", idx = "index")
expect_identical(
  runner(x, f = function(x) mean(x$x)),
  runner(data, f = function(x) mean(x$x), at = "index", idx = "index")
)

x <- run_by(data, at = 1:3, idx = "index")
expect_error(
  runner(x, f = function(x) mean(x$x), at = "index2"),
  "`at` is invalid"
)

x <- run_by(data, at = "index")
names(x)[1] <- "wrong"
expect_error(runner(x, f = function(x) mean(x$x)), "`at` is invalid")


# all
x <- run_by(data, idx = "index", k = 5, lag = 2, na_pad = FALSE, at = 10)
expect_identical(
  runner(x, f = function(x) mean(x$x)),
  runner(data, f = function(x) mean(x$x), idx = data$index, k = 5, lag = 2, na_pad = FALSE, at = 10)
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
  "`idx` is invalid"
)

x <- run_by(data, idx = "index")
expect_error(
  runner(
    x,
    idx = "index2",
    f = function(x) mean(x$x)
  ),
  "`idx` is invalid"
)


x <- run_by(data, at = "index")
expect_error(
  runner(
    x,
    at = "1 day",
    f = function(x) mean(x$x)
  ),
  "`at` is invalid"
)

expect_error(
  run_by(data, k = as.factor(1:10)),
  "`k` is invalid, should be either:"
)

expect_error(
  run_by(data, idx = as.factor(1:10)),
  "`idx` is invalid"
)

expect_error(
  run_by(data, at = as.factor(1:10)),
  "`at` is invalid"
)

expect_error(
  run_by(data, lag = "1 day"),
  "`lag` is invalid, should be either"
)

expect_error(
  runner(
    data,
    k = "-1 day",
    f = function(x) mean(x$x)
  ),
  "`k` is invalid, should be either"
)
