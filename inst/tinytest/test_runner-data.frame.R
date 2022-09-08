elo <- data.frame(
  index = 1:100,
  group = rep(c("a", "b"), each = 50),
  a = sample(letters, 100, replace = TRUE),
  b = 1:100,
  k = sample(1:5, 100, replace = TRUE)
)
index <- 101:200

expect_equal(
  res <- runner(elo, k = 10, lag = 1, f = function(x) x[1, 1], simplify = TRUE),
  runner(
    seq_len(nrow(elo)),
    k = 10,
    lag = 1,
    f = function(idx) if (length(idx) == 0 || all(is.na(idx))) {
      NA
    } else {
      elo[idx, 1][1]
    },
    simplify = TRUE
  )
)
expect_true(is(res, "integer"))

expect_equal(
  res <- runner(elo, k = 10, lag = 1,
                f = function(x) x[1, 1], simplify = FALSE),
  runner(
    seq_len(nrow(elo)),
    k = 10,
    lag = 1,
    f = function(idx) if (length(idx) == 0 || all(is.na(idx))) {
      NA
    } else {
      elo[idx, 1][1]
    },
    simplify = FALSE
  )
)
expect_true(is(res, "list"))

expect_equal(
  runner(elo, k = 10, lag = 1, f = function(x) x)[[50]],
  elo[40:49, ]
)

expect_equal(
  runner(elo, k = 10, lag = 1, f = function(x) x)[[1]],
  NA
)

expect_equal(
  runner(elo, k = 10, lag = 1, f = function(x) x)[[2]],
  elo[1, ]
)

#### more tests
elo <- data.frame(
  index = 1:100,
  group = rep(c("a", "b"), each = 50),
  a = sample(letters, 100, replace = TRUE),
  b = 1:100,
  k = sample(1:5, 100, replace = TRUE)
)

expect_error(
  runner(
    transform(elo, k = sample(c(1, 2, NA), 100, replace = TRUE)),
    k = "k"
  )
)

expect_error(
  runner(
    transform(elo, lag = sample(c(1, 2, NA), 100, replace = TRUE)),
    lag = "lag"
  )
)

expect_error(
  runner(
    transform(elo, idx = sample(c(1, 2, NA), 100, replace = TRUE)),
    idx = "idx"
  )
)

expect_error(
  runner(
    transform(elo, at = sample(c(1, 2, NA), 100, replace = TRUE)),
    at = "at"
  )
)

expect_error(
  runner(
    elo,
    f = NULL
  )
)

####
expect_error(
  runner(
    elo,
    k = "1 days"
  ),
  "idx` can't be empty"
)

expect_error(
  runner(
    elo,
    lag = rep("1 days", 100)
  ),
  "idx` can't be empty"
)

expect_error(
  runner(
    elo,
    k = as.difftime(1, units = "days")
  ),
  "idx` can't be empty"
)

expect_error(
  runner(
    elo,
    lag = as.difftime(rep(1, 100), units = "days")
  ),
  "idx` can't be empty"
)

expect_error(
  runner(
    elo,
    lag = factor(1:100)
  ),
  "`lag` is invalid"
)
