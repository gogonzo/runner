set.seed(11)
x1 <- sample(c(1, 2, 3), 100, replace = TRUE)
x2 <- sample(c(NA, 1, 2, 3), 100, replace = TRUE)
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
streak2 <- function(x, na_rm = TRUE) {  #nolint
  if (all(is.na(x))) return(NA)
  xx <- x[length(x)]
  astreak <- 0L
  if (na_rm) {
    for (i in rev(seq_along(x))) {
      if (is.na(x[i])) next
      if (is.na(xx)) xx <- x[i]
      if (x[i] != xx) break
      astreak <- astreak + 1L
    }
  } else {
    for (i in rev(seq_along(x))) {
      if (is.na(x[i])) return(NA)
      if (x[i] != xx) break
      astreak <- astreak + 1L
    }
  }

  if (astreak == 0) return(NA)
  return(astreak)
}

#       |--------]-------> ------
expect_identical(
  streak_run(x2),
  as.integer(runner(x2, f = streak2))
)

expect_identical(
  streak_run(x2, na_pad = TRUE),
  as.integer(runner(x2, f = streak2, na_pad = TRUE))
)

#   [...|----]---+-------> ------
expect_equal(
  streak_run(x2, lag = 3),
  runner(x2, lag = 3, f = streak2))

expect_equal(
  streak_run(x2, lag = 3, na_pad = TRUE),
  runner(x2, lag = 3, f = streak2, na_pad = TRUE))

#       |--------+---]---> ------
expect_equal(
  streak_run(x2, lag = -3),
  runner(x2, lag = -3, f = streak2))

expect_equal(
  streak_run(x2, lag = -3, na_pad = TRUE),
  runner(x2, lag = -3, f = streak2, na_pad = TRUE))

#  [...]|--------+-------> ------
expect_equal(
  streak_run(x2, lag = 100),
  suppressWarnings(runner(x2, lag = 100, f = streak2, type = "numeric"))
)

expect_equal(
  streak_run(x2, lag = 100, na_pad = TRUE),
  suppressWarnings(
    runner(x2, lag = 100, f = streak2, na_pad = TRUE, type = "numeric")
  )
)


expect_equal(
  streak_run(x2, lag = -100),
  runner(x2, lag = -100, f = streak2))

expect_equal(
  streak_run(x2, lag = -100, na_pad = TRUE),
  suppressWarnings(
    runner(x2, lag = -100, f = streak2, na_pad = TRUE, type = "numeric")
    )
)

#       |----[...]-------> ------
expect_equal(
  streak_run(x2, k = 3),
  runner(x2, k = 3, f = streak2))

expect_equal(
  streak_run(x2, k = 3, na_pad = TRUE),
  runner(x2, k = 3, f = streak2, na_pad = TRUE))

#       [...|--------+-------[...] ------
expect_equal(
  streak_run(x2, k = 1),
  runner(x2, k = 1, f = streak2))

expect_equal(
  streak_run(x2, k = 1, na_pad = TRUE),
  runner(x2, k = 1, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 99),
  runner(x2, k = 99, f = streak2))

expect_equal(
  streak_run(x2, k = 99, na_pad = TRUE),
  runner(x2, k = 99, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 100),
  runner(x2, k = 100, f = streak2))

expect_equal(
  streak_run(x2, k = 100, na_pad = TRUE),
  runner(x2, k = 100, f = streak2, na_pad = TRUE))

#       [...|----]---+-------> ------
expect_equal(
  streak_run(x2, k = 5, lag = 3),
  runner(x2, k = 5, lag = 3, f = streak2))

expect_equal(
  streak_run(x2, k = 5, lag = 3, na_pad = TRUE),
  runner(x2, k = 5, lag = 3, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 5, lag = 3, na_rm = FALSE),
  runner(x2, k = 5, lag = 3, f = function(x) streak2(x, na_rm = FALSE)))

expect_equal(
  streak_run(x2, k = 5, lag = 3, na_pad = TRUE, na_rm = FALSE),
  runner(x2, k = 5, lag = 3,
         f = function(x) streak2(x, na_rm = FALSE), na_pad = TRUE))

#       |-----[--+---]---> ------
expect_equal(
  streak_run(x2, k = 5, lag = -3),
  runner(x2, k = 5, lag = -3, f = streak2))

expect_equal(
  streak_run(x2, k = 5, lag = -3, na_pad = TRUE),
  runner(x2, k = 5, lag = -3, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 5, lag = -3, na_rm = FALSE),
  runner(x2, k = 5, lag = -3, f = function(x) streak2(x, na_rm = FALSE)))

expect_equal(
  streak_run(x2, k = 5, lag = -3, na_pad = TRUE, na_rm = FALSE),
  runner(x2, k = 5, lag = -3,
         f = function(x) streak2(x, na_rm = FALSE), na_pad = TRUE))

#       |--------+-[---]-> ------
expect_equal(
  streak_run(x2, k = 5, lag = -7),
  runner(x2, k = 5, lag = -7, f = streak2))

expect_equal(
  streak_run(x2, k = 5, lag = -7, na_pad = TRUE),
  runner(x2, k = 5, lag = -7, f = streak2, na_pad = TRUE))

#       |--------+[]-----> ------
expect_equal(
  streak_run(x2, k = 1, lag = -1),
  runner(x2, k = 1, lag = -1, f = streak2))

expect_equal(
  streak_run(x2, k = 1, lag = -1, na_pad = TRUE),
  runner(x2, k = 1, lag = -1, f = streak2, na_pad = TRUE))

#       |------[]+-------> ------
expect_equal(
  streak_run(x2, k = 1, lag = 1),
  runner(x2, k = 1, lag = 1, f = streak2))

expect_equal(
  streak_run(x2, k = 1, lag = 1, na_pad = TRUE),
  runner(x2, k = 1, lag = 1, f = streak2, na_pad = TRUE))

#various ------
expect_equal(
  streak_run(x2, k = k, lag = 1),
  runner(x2, k = k, lag = 1, f = streak2))

expect_equal(
  streak_run(x2, k = k, lag = 1, na_pad = TRUE),
  runner(x2, k = k, lag = 1, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, k = 3, lag = lag),
  runner(x2, k = 3, lag = lag, f = streak2))

expect_equal(
  streak_run(x2, k = 3, lag = lag, na_pad = TRUE),
  runner(x2, k = 3, lag = lag, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = k, lag = lag),
  runner(x2, k = k, lag = lag, f = streak2))

expect_equal(
  streak_run(x2, k = k, lag = lag, na_pad = TRUE),
  runner(x2, k = k, lag = lag, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = k, lag = lag, na_rm = FALSE),
  runner(x2, k = k, lag = lag, f = function(x) streak2(x, na_rm = FALSE)))

expect_equal(
  streak_run(x2, k = k, lag = lag, na_rm = FALSE, na_pad = TRUE),
  runner(x2, k = k, lag = lag,
         f = function(x) streak2(x, na_rm = FALSE), na_pad = TRUE))

#date window ------
expect_equal(
  streak_run(x2, lag = 3, idx = idx, na_pad = FALSE),
  runner(x2, lag = 3, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = 3, idx = idx, na_pad = TRUE),
  runner(x2, lag = 3, idx = idx, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, lag = -3, idx = idx, na_pad = FALSE),
  runner(x2, lag = -3, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = -3, idx = idx, na_pad = TRUE),
  runner(x2, lag = -3, idx = idx, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 3, idx = idx, na_pad = FALSE),
  runner(x2, k = 3, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, k = 3, idx = idx, na_pad = TRUE),
  runner(x2, k = 3, idx = idx, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, lag = -1, idx = idx, na_pad = FALSE),
  runner(x2, lag = -1, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = -1, idx = idx, na_pad = TRUE),
  runner(x2, lag = -1, idx = idx, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, lag = 100, idx = idx, na_pad = FALSE),
  runner(x2, lag = 100, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = 100, idx = idx, na_pad = TRUE),
  runner(x2, lag = 100, idx = idx, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, lag = -100, idx = idx, na_pad = FALSE),
  runner(x2, lag = -100, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = -100, idx = idx, na_pad = TRUE),
  runner(x2, lag = -100, idx = idx, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, lag = lag, idx = idx, na_pad = FALSE),
  runner(x2, lag = lag, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, lag = lag, idx = idx, na_pad = TRUE),
  runner(x2, lag = lag, idx = idx, f = streak2, na_pad = TRUE))

expect_equal(
  streak_run(x2, k = 3, lag = 4, idx = idx, na_pad = FALSE),
  runner(x2, k = 3, lag = 4, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, k = 3, lag = 4, idx = idx, na_pad = TRUE),
  runner(x2, k = 3, lag = 4, idx = idx, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, k = 3, lag = -4, idx = idx, na_pad = FALSE),
  runner(x2, k = 3, lag = -4, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, k = 3, lag = -4, idx = idx, na_pad = TRUE),
  runner(x2, k = 3, lag = -4, idx = idx, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, k = k, lag = -4, idx = idx, na_pad = FALSE),
  runner(x2, k = k, lag = -4, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, k = k, lag = -4, idx = idx, na_pad = TRUE),
  runner(x2, k = k, lag = -4, idx = idx, f = streak2, na_pad = TRUE))


expect_equal(
  streak_run(x2, k = 4, lag = lag, idx = idx, na_pad = FALSE),
  runner(x2, k = 4, lag = lag, idx = idx, f = streak2, na_pad = FALSE))

expect_equal(
  streak_run(x2, k = 4, lag = lag, idx = idx, na_pad = TRUE),
  runner(x2, k = 4, lag = lag, idx = idx, f = streak2, na_pad = TRUE))

#data types ------
expect_equal(
  streak_run(as.integer(x2)),
  runner(as.integer(x2), f = streak2))

expect_equal(
  streak_run(as.character(x2)),
  runner(as.character(x2), f = streak2))

expect_equal(
  streak_run(c(TRUE, TRUE, FALSE, FALSE, TRUE)),
  runner(c(TRUE, TRUE, FALSE, FALSE, TRUE), f = streak2))

expect_equal(
  streak_run(as.integer(x2), k = 2),
  runner(as.integer(x2), k = 2, f = streak2))

expect_equal(
  streak_run(as.character(x2), k = 2),
  runner(as.character(x2), k = 2, f = streak2))

expect_equal(
  streak_run(c(TRUE, TRUE, FALSE, FALSE, TRUE), k = 2),
  runner(c(TRUE, TRUE, FALSE, FALSE, TRUE), k = 2, f = streak2))


#Errors ------
expect_error(streak_run(x1, k = (1:999)),
             "length of k and length of x differs")
expect_error(streak_run(x1, k = c(NA, k[-1])),
             "Function doesn't accept NA values in k vector")

expect_error(streak_run(x1, lag = (1:99)),
             "length of lag and length of x differs")
expect_error(streak_run(x1, lag = c(NA, lag[-1])),
             "Function doesn't accept NA values in lag vector")

expect_error(streak_run(x1, idx = (1:99)),
             "length of idx and length of x differs")
expect_error(streak_run(x1, idx = c(NA, 1:99)),
             "Function doesn't accept NA values in idx vector")
