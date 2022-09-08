df <- data.frame(
  a = 1:13,
  b = 1:13 + rnorm(13, sd = 5),
  idx = seq(Sys.Date(), Sys.Date() + 365, by = "1 month")
)
tinytest::expect_identical(
  do.call(
    "runner::runner",
    list(
      x = df,
      idx = "idx",
      at = "6 months",
      f = function(x) {
        cor(x$a, x$b)
      }
    )
  )
)

elo <- data.frame(
  index = 1:100,
  group = rep(c("a", "b"), each = 50),
  a = sample(letters, 100, replace = TRUE),
  b = 1:100,
  k = sample(1:5, 100, replace = TRUE)
)
index <- 101:200

tinytest::expect_equal(
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
tinytest::expect_true(is(res, "integer"))

tinytest::expect_equal(
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
tinytest::expect_true(is(res, "list"))

tinytest::expect_equal(
  runner(elo, k = 10, lag = 1, f = function(x) x)[[50]],
  elo[40:49, ]
)

tinytest::expect_equal(
  runner(elo, k = 10, lag = 1, f = function(x) x)[[1]],
  NA
)

tinytest::expect_equal(
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

tinytest::expect_error(
  runner(
    transform(elo, k = sample(c(1, 2, NA), 100, replace = TRUE)),
    k = "k"
  )
)

tinytest::expect_error(
  runner(
    transform(elo, lag = sample(c(1, 2, NA), 100, replace = TRUE)),
    lag = "lag"
  )
)

tinytest::expect_error(
  runner(
    transform(elo, idx = sample(c(1, 2, NA), 100, replace = TRUE)),
    idx = "idx"
  )
)

tinytest::expect_error(
  runner(
    transform(elo, at = sample(c(1, 2, NA), 100, replace = TRUE)),
    at = "at"
  )
)

tinytest::expect_error(
  runner(
    elo,
    f = NULL
  )
)

####
tinytest::expect_error(
  runner(
    elo,
    k = "1 days"
  ),
  "idx` can't be empty"
)

tinytest::expect_error(
  runner(
    elo,
    lag = rep("1 days", 100)
  ),
  "idx` can't be empty"
)

tinytest::expect_error(
  runner(
    elo,
    k = as.difftime(1, units = "days")
  ),
  "idx` can't be empty"
)

tinytest::expect_error(
  runner(
    elo,
    lag = as.difftime(rep(1, 100), units = "days")
  ),
  "idx` can't be empty"
)

tinytest::expect_error(
  runner(
    elo,
    lag = factor(1:100)
  ),
  "`lag` is invalid"
)
