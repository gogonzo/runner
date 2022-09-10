set.seed(as.integer(Sys.Date()))
x1 <- x2 <- 1:100
x2[sample(1:100, 10)] <- NA
k <- sample(1:100, 100, replace = TRUE)
lag <- sample(-15:15, 100, replace = TRUE)
idx <- cumsum(sample(c(1, 2, 3, 4), 100, replace = TRUE))
idx_date <- Sys.Date() + idx
at <- sample(1:100, 10)
at_date <- sample(idx, 10)
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
apply_fun <- function(x, i, k, lag = 0, na_pad = FALSE, fun, ...) {
  n <- length(x)
  values <- x[find_idx(x, i, k, lag, na_pad)]
  if (length(values) > 0) {
    fun(values, ...)
  } else {
    NA
  }
}

#       |--------]-------> ------
expect_identical(
  runner(x1, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, fun = mean)))

expect_identical(
  runner(x2, f = mean, na.rm = TRUE),
  sapply(
    seq_along(x2),
    function(i) apply_fun(x2, i = i, fun = mean, na.rm = TRUE)
  )
)

expect_identical(
  runner(x1, f = mean, idx = idx),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, fun = mean)))


expect_identical(
  runner(x1, f = mean)[at],
  runner(x1, f = mean, at = at))

expect_identical(
  runner(x1, f = mean, idx = idx)[at],
  runner(x1, f = mean, at = at))

#   [...|----]---+-------> -------
expect_equal(
  runner(x1, lag = 3, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, lag = 3, fun = mean)))

expect_equal(
  runner(x1, lag = 3, f = mean, na_pad = TRUE),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, lag = 3, fun = mean)))

expect_equal(
  runner(x1, lag = 3, f = mean)[at],
  runner(x1, lag = 3, f = mean, at = at))

expect_equal(
  runner(x1, lag = 3, f = mean, na_pad = TRUE)[at],
  runner(x1, lag = 3, f = mean, na_pad = TRUE, at = at))

#       |--------+---]---> -------
expect_equal(
  runner(x1, lag = -3, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, lag = -3, fun = mean)))

expect_equal(
  runner(x1, lag = -3, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, lag = -3, fun = mean, na_pad = TRUE)
  )
)

expect_equal(
  runner(x1, lag = lag, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, lag = lag[i], fun = mean)
  )
)

expect_equal(
  runner(x1, lag = lag, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, lag = lag[i], na_pad = TRUE, fun = mean)
  )
)

expect_equal(
  runner(x1, lag = -3, f = mean)[at],
  runner(x1, lag = -3, f = mean, at = at))

expect_equal(
  runner(x1, lag = -3, f = mean, na_pad = TRUE)[at],
  runner(x1, lag = -3, f = mean, na_pad = TRUE, at = at))

expect_equal(
  runner(x1, lag = lag, f = mean)[at],
  runner(x1, lag = lag[at], f = mean, at = at))

expect_equal(
  runner(x1, lag = lag, f = mean, na_pad = TRUE)[at],
  runner(x1, lag = lag[at], f = mean, na_pad = TRUE, at = at))

#  [...]|--------+-------> -------
expect_equal(
  runner(x1, lag = 100, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, lag = 100, fun = mean))
)

expect_equal(
  runner(x1, lag = -100, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, lag = -100, fun = mean)))

expect_equal(
  runner(x1, lag = 100, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, lag = 100, na_pad = TRUE, fun = mean)
  )
)

expect_equal(
  runner(x1, lag = -100, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, lag = -100, na_pad = TRUE, fun = mean)
  )
)

expect_equal(
  runner(x1, lag = 100, f = mean)[at],
  runner(x1, lag = 100, f = mean, at = at))

expect_equal(
  runner(x1, lag = -100, f = mean)[at],
  runner(x1, lag = -100, f = mean, at = at))

expect_equal(
  runner(x1, lag = 100, f = mean, na_pad = TRUE)[at],
  runner(x1, lag = 100, f = mean, na_pad = TRUE, at = at))

expect_equal(
  runner(x1, lag = -100, f = mean, na_pad = TRUE)[at],
  runner(x1, lag = -100, f = mean, na_pad = TRUE, at = at))

#       |----[...]-------> -------
expect_equal(
  runner(x1, k = 3, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(x1, i = i, k = 3, fun = mean)))

expect_equal(
  runner(x1, k = 3, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, k = 3, na_pad = TRUE, fun = mean)
  )
)

expect_equal(
  runner(x1, k = k, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(x1, i = i, k = k[i], lag = 0, fun = mean)
    )
  )

expect_equal(
  runner(x1, k = 3, f = mean)[at],
  runner(x1, k = 3, f = mean, at = at))

expect_equal(
  runner(x1, k = 3, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 3, f = mean, na_pad = TRUE, at = at))

expect_equal(
  runner(x1, k = k, f = mean)[at],
  runner(x1, k = k[at], f = mean, at = at))

#       [...|--------+-------[...] -------
expect_equal(
  runner(x1, k = 100, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(fun = mean, x1, i = i, k = 100)))

expect_equal(
  runner(x1, k = 100, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 100, na_pad = TRUE)
  )
)

expect_equal(
  runner(x1, k = 101, f = mean),
  sapply(seq_along(x1), function(i) apply_fun(fun = mean, x1, i = i, k = 101)))

expect_equal(
  runner(x1, k = 101, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 101, na_pad = TRUE)
  )
)

expect_equal(
  runner(x1, k = 100, f = mean)[at],
  runner(x1, k = 100, f = mean, at = at))

expect_equal(
  as.numeric(runner(x1, k = 100, f = mean, na_pad = TRUE)[at]),
  as.numeric(runner(x1, k = 100, f = mean, na_pad = TRUE, at = at)))

expect_equal(
  runner(x1, k = 101, f = mean)[at],
  runner(x1, k = 101, f = mean, at = at))

expect_equal(
  runner(x1, k = 101, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 101, f = mean, na_pad = TRUE, at = at))

#       [...|----]---+-------> -------
expect_equal(
  runner(x1, k = 5, lag = 3, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = 3)
    )
  )

expect_equal(
  runner(x1, k = 5, lag = 3, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = 3, na_pad = TRUE)
  )
)

expect_equal(
  runner(x1, k = 5, lag = 3, f = mean)[at],
  runner(x1, k = 5, lag = 3, f = mean, at = at))

expect_equal(
  runner(x1, k = 5, lag = 3, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 5, lag = 3, f = mean, na_pad = TRUE, at = at))

#       |-----[--+---]---> -------
expect_equal(
  runner(x1, k = 5, lag = -3, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = -3)
    )
  )

expect_equal(
  runner(x1, k = 5, lag = -3, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = -3, na_pad = TRUE)
    )
  )



expect_equal(
  runner(x1, k = 5, lag = -3, f = mean)[at],
  runner(x1, k = 5, lag = -3, f = mean, at = at))

expect_equal(
  runner(x1, k = 5, lag = -3, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 5, lag = -3, f = mean, na_pad = TRUE, at = at))

#       |--------+-[---]-> -------
expect_equal(
  runner(x1, k = 5, lag = -7, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = -7)
    )
)

expect_equal(
  runner(x1, k = 5, lag = -7, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 5, lag = -7, na_pad = TRUE)
    )
  )


expect_equal(
  runner(x1, k = 5, lag = -7, f = mean)[at],
  runner(x1, k = 5, lag = -7, f = mean, at = at))

expect_equal(
  runner(x1, k = 5, lag = -7, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 5, lag = -7, f = mean, na_pad = TRUE, at = at))

#       |--------+[]-----> -------
expect_equal(
  runner(x1, k = 1, lag = -1, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 1, lag = -1)
    )
  )

expect_equal(
  runner(x1, k = 1, lag = -1, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 1, lag = -1, na_pad = TRUE)
    )
  )



expect_equal(
  runner(x1, k = 1, lag = -1, f = mean)[at],
  runner(x1, k = 1, lag = -1, f = mean, at = at))

expect_equal(
  runner(x1, k = 1, lag = -1, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 1, lag = -1, f = mean, na_pad = TRUE, at = at))

#       |------[]+-------> -------
expect_equal(
  runner(x1, k = 1, lag = 1, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 1, lag = 1)
    )
  )

expect_equal(
  runner(x1, k = 1, lag = 1, f = mean, na_pad = TRUE),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 1, lag = 1, na_pad = TRUE)
    )
  )


expect_equal(
  runner(x1, k = 1, lag = 1, f = mean)[at],
  runner(x1, k = 1, lag = 1, f = mean, at = at))

expect_equal(
  runner(x1, k = 1, lag = 1, f = mean, na_pad = TRUE)[at],
  runner(x1, k = 1, lag = 1, f = mean, na_pad = TRUE, at = at))

#various -------
expect_equal(
  runner(x1, k = k, lag = 1, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = k[i], lag = 1)
    )
  )

expect_equal(
  runner(x1, k = 3, lag = lag, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = 3, lag = lag[i])
    )
  )

expect_equal(
  runner(x1, k = length(x1), lag = lag, f = mean),
  sapply(
    seq_along(x1),
    function(i) apply_fun(fun = mean, x1, i = i, k = length(x1), lag = lag[i])
    )
  )

expect_equal(
  runner(x1, k = k, lag = lag, f = mean),
  sapply(window_run(x1, k = k, lag = lag), mean))



expect_equal(
  runner(x1, k = k, lag = 1, f = mean)[at],
  runner(x1, k = k[at], lag = 1, f = mean, at = at))

expect_equal(
  runner(x1, k = 3, lag = lag, f = mean)[at],
  runner(x1, k = 3, lag = lag[at], f = mean, at = at))

expect_equal(
  runner(x1, k = length(x1), lag = lag, f = mean)[at],
  runner(x1, k = length(x1), lag = lag[at], f = mean, at = at))

expect_equal(
  runner(x1, k = k, lag = lag, f = mean)[at],
  runner(x1, k = k[at], lag = lag[at], f = mean, at = at))

#date window -------
expect_equal(
  runner(x1, idx = idx, f = mean),
  sapply(window_run(x1, idx = idx), mean)
)

expect_equal(
  runner(x2, idx = idx, f = mean, na_pad = TRUE),
  sapply(window_run(x2, idx = idx, na_pad = TRUE), mean)
)

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

expect_equal(
  runner(x1, k = k, lag = lag, idx = idx, f = mean),
  sapply(window_run(x1, k = k, lag = lag, idx = idx), mean)
)

expect_equal(
  runner(x2, k = k, lag = 3, idx = idx, f = mean, na_pad = TRUE),
  sapply(window_run(x2, k = k, lag = 3, idx = idx, na_pad = TRUE), mean)
)

#Function applied on other types -------
expect_silent(runner(as.integer(1:100), k = 5, f = length))
expect_silent(runner(as.integer(1:100), k = k, f = length))
expect_silent(runner(as.integer(1:100), k = k, idx, f = length))

expect_silent(
  runner(sample(letters, 100, replace = TRUE), k = 5, f = length)
)
expect_silent(runner(sample(letters, 100, replace = TRUE), k = k, f = length))
expect_silent(
  runner(sample(letters, 100, replace = TRUE), k = k, idx, f = length)
  )

expect_silent(
  runner(as.factor(sample(letters, 100, replace = TRUE)), k = 5, f = length)
)
expect_silent(
  runner(as.factor(sample(letters, 100, replace = TRUE)), k = k, f = length)
)
expect_silent(
  runner(
    as.factor(sample(letters, 100, replace = TRUE)), k = k, idx, f = length
  )
)

expect_silent(
  runner(as.Date(1:100, origin = "1970-01-01"), k = 5, f = length)
)
expect_silent(
  runner(as.Date(1:100, origin = "1970-01-01"), k = k, f = length)
  )
expect_silent(
  runner(as.Date(1:100, origin = "1970-01-01"), k = k, idx, f = length)
  )

#i/o type -------
log_input <- c(T, T, F, F)
int_input <- as.integer(1:4)
num_input <- as.numeric(1:4) + 0.5
cha_input <- letters[1:4]

log_function <- function(x) any(duplicated(x))
int_function  <- function(x) as.integer(length(x))
num_function  <- function(x) as.double(sum(x))
char_function <- function(x) paste(x, collapse = "-")

# <logical>
expect_identical(
  as.logical(c(FALSE, TRUE, TRUE, TRUE)),
  runner(log_input, f = log_function)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE)),
  runner(int_input, f = log_function)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE)),
  runner(num_input, f = log_function)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE)),
  runner(cha_input, f = log_function)
)

# <integer>

expect_identical(
  as.integer(c(1, 2, 3, 4)),
  runner(log_input, f = int_function)
)

expect_identical(
  as.integer(c(1, 2, 3, 4)),
  runner(int_input, f = int_function)
)

expect_identical(
  as.integer(c(1, 2, 3, 4)),
  runner(num_input, f = int_function)
)

expect_identical(
  as.integer(c(1, 2, 3, 4)),
  runner(cha_input, f = int_function)
)

# <numeric>
expect_identical(
  as.numeric(c(1, 2, 2, 2)),
  runner(log_input, f = num_function)
)

expect_identical(
  as.numeric(c(1, 3, 6, 10)),
  runner(int_input, f = num_function)
)

expect_identical(
  as.numeric(c(1.5, 4, 7.5, 12)),
  runner(num_input, f = num_function))

expect_error(runner(cha_input, f = num_function))

# <character>
expect_identical(
  c("TRUE", "TRUE-TRUE", "TRUE-TRUE-FALSE", "TRUE-TRUE-FALSE-FALSE"),
  runner(log_input, f = char_function)
)

expect_identical(
  c("1", "1-2", "1-2-3", "1-2-3-4"),
  runner(int_input, f = char_function)
)

expect_identical(
  c("1.5", "1.5-2.5", "1.5-2.5-3.5", "1.5-2.5-3.5-4.5"),
  runner(num_input, f = char_function)
)

expect_identical(
  c("a", "a-b", "a-b-c", "a-b-c-d"),
  runner(cha_input, f = char_function)
)

#i/o type at -------
log_input <- c(T, T, F, F)
int_input <- as.integer(1:4)
num_input <- as.numeric(1:4) + 0.5
cha_input <- letters[1:4]

log_function <- function(x) any(duplicated(x))
int_function  <- function(x) as.integer(length(x))
num_function  <- function(x) as.double(sum(x))
char_function <- function(x) paste(x, collapse = "-")

at <- c(2, 3)
# <logical>
expect_identical(
  as.logical(c(FALSE, TRUE, TRUE, TRUE))[at],
  runner(log_input, f = log_function, at = at)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE))[at],
  runner(int_input, f = log_function, at = at)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE))[at],
  runner(num_input, f = log_function, at = at)
)

expect_identical(
  as.logical(c(FALSE, FALSE, FALSE, FALSE))[at],
  runner(cha_input, f = log_function, at = at)
)
# <integer>
expect_identical(
  as.integer(c(1, 2, 3, 4))[at],
  runner(log_input, f = int_function, at = at)
)
expect_identical(
  as.integer(c(1, 2, 3, 4))[at],
  runner(int_input, f = int_function, at = at)
)
expect_identical(
  as.integer(c(1, 2, 3, 4))[at],
  runner(num_input, f = int_function, at = at)
)
expect_identical(
  as.integer(c(1, 2, 3, 4))[at],
  runner(cha_input, f = int_function, at = at)
)

# <numeric>
expect_identical(
  as.numeric(c(1, 2, 2, 2))[at],
  runner(log_input, f = num_function, at = at)
)
expect_identical(
  as.numeric(c(1, 3, 6, 10))[at],
  runner(int_input, f = num_function, at = at)
)
expect_identical(
  as.numeric(c(1.5, 4, 7.5, 12))[at],
  runner(num_input, f = num_function, at = at)
)
expect_error(runner(cha_input, f = num_function, at = at))
# <character>
expect_identical(
  c("TRUE", "TRUE-TRUE", "TRUE-TRUE-FALSE", "TRUE-TRUE-FALSE-FALSE")[at],
  runner(log_input, f = char_function, at = at)
)
expect_identical(
  c("1", "1-2", "1-2-3", "1-2-3-4")[at],
  runner(int_input, f = char_function, at = at)
)

expect_identical(
  c("1.5", "1.5-2.5", "1.5-2.5-3.5", "1.5-2.5-3.5-4.5")[at],
  runner(num_input, f = char_function, at = at)
)
expect_identical(
  c("a", "a-b", "a-b-c", "a-b-c-d")[at],
  runner(cha_input, f = char_function, at = at)
)

#at date window -------
ids <- match(at_date, idx)
expect_equal(
  runner(x1, idx = idx, f = mean)[ids],
  runner(x1, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x1, idx = idx, f = mean)[ids],
  runner(x1, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x1, lag = 3, idx = idx, f = mean, na_pad = FALSE)[ids],
  runner(x1, lag = 3, idx = idx, at = at_date, f = mean, na_pad = FALSE)
)

expect_equal(
  runner(x1, lag = 3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x1, lag = 3, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x2, lag = 3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, lag = 3, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, lag = -3, idx = idx, f = mean)[ids],
  runner(x1, lag = -3, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, lag = -3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, lag = -3, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, lag = -1, idx = idx, f = mean)[ids],
  runner(x1, lag = -1, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, lag = -1, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, lag = -1, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, lag = -100, idx = idx, f = mean)[ids],
  runner(x1, lag = -100, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, lag = -100, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, lag = -100, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, lag = lag, idx = idx, f = mean)[ids],
  runner(x1, lag = lag[ids], idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, lag = -lag, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, lag = -lag[ids], idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, k = 3, lag = 3, idx = idx, f = mean)[ids],
  runner(x1, k = 3, lag = 3, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = 3, lag = 3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = 3, lag = 3, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, k = 3, lag = -3, idx = idx, f = mean)[ids],
  runner(x1, k = 3, lag = -3, idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = 3, lag = -3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = 3, lag = -3, idx = idx, at = at_date, f = mean, na_pad = TRUE)
)


expect_equal(
  runner(x1, k = 3, lag = lag, idx = idx, f = mean)[ids],
  runner(x1, k = 3, lag = lag[ids], idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = 3, lag = lag, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = 3, lag = lag[ids], idx = idx,
         at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, k = k, lag = lag, idx = idx, f = mean)[ids],
  runner(x1, k = k[ids], lag = lag[ids], idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = k, lag = lag, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = k[ids], lag = lag[ids], idx = idx,
         at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, k = 3, lag = lag, idx = idx, f = mean)[ids],
  runner(x1, k = 3, lag = lag[ids], idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = k, lag = 3, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = k[ids], lag = 3, idx = idx,
         at = at_date, f = mean, na_pad = TRUE)
)

expect_equal(
  runner(x1, k = k, lag = lag, idx = idx, f = mean)[ids],
  runner(x1, k = k[ids], lag = lag[ids], idx = idx, at = at_date, f = mean)
)

expect_equal(
  runner(x2, k = k, lag = lag, idx = idx, f = mean, na_pad = TRUE)[ids],
  runner(x2, k = k[ids], lag = lag[ids], idx = idx,
         at = at_date, f = mean, na_pad = TRUE)
)

#at with difftime -------
at_date <- runner:::.seq_at(at = "1 months", idx = idx_date)
expect_identical(
  at_date,
  seq(min(idx_date), max(idx_date), by = "1 months")
)

expect_equal(
  runner(1:100, at = "1 months", idx = idx_date, f = function(x) max(x)),
  runner(1:100, at = at_date, idx = idx_date, f = function(x) max(x))
)

at_date <- runner:::.seq_at(at = "-1 months", idx = idx_date)
expect_identical(
  at_date,
  seq(max(idx_date), min(idx_date), by = "-1 months")
)
expect_equal(
  runner(1:100, at = "-1 months", idx = idx_date, f = function(x) max(x)),
  runner(1:100, at = at_date, idx = idx_date, f = function(x) max(x))
)

#k with difftime -------
expect_equal(
  runner(1:100, k = "2 weeks", idx = idx_date, f = function(x) x),
  runner(1:100, k = 14, idx = idx_date, f = function(x) x)
)

expect_equal(
  runner(1:100, k = "2 weeks", idx = idx_date, f = function(x) x),
  runner(1:100, k = as.difftime(2, units = "weeks"),
         idx = idx_date, f = function(x) x)
)

k_date <- sample(c("week", "day"), 100, replace = TRUE)
k_int <- ifelse(k_date == "week", 7L, 1L)

expect_equal(
  runner(1:100, k = k_date, idx = idx_date, f = function(x) x),
  runner(1:100, k = k_int, idx = idx_date, f = function(x) x)
)

expect_error(
  runner(1:100, k = "-1 weeks", idx = idx_date, f = function(x) x),
  "negative"
)

expect_error(
  runner(1:100,
         k = as.difftime(-1, units = "weeks"),
         idx = idx_date,
         f = function(x) x),
  "negative"
)

#lag with difftime -------
expect_equal(
  runner(1:100, lag = "week", idx = idx_date, f = function(x) x),
  runner(1:100, lag = 7, idx = idx_date, f = function(x) x)
)

expect_equal(
  runner(1:100, lag = "-1 weeks", idx = idx_date, f = function(x) x),
  runner(1:100, lag = -7, idx = idx_date, f = function(x) x)
)


expect_equal(
  runner(1:100, lag = "2 weeks", idx = idx_date, f = function(x) x),
  runner(1:100,
         lag = as.difftime(2, units = "weeks"),
         idx = idx_date,
         f = function(x) x)
)

expect_equal(
  runner(1:100, lag = "-2 weeks", idx = idx_date, f = function(x) x),
  runner(1:100,
         lag = as.difftime(-2, units = "weeks"),
         idx = idx_date,
         f = function(x) x)
)


lag_date <- sample(c("week", "day", "-2 weeks", "-2 days"), 100, replace = TRUE)
lag_int <- vapply(
  lag_date,
  function(x)
    switch(x, "week" = 7L, "day" = 1L, "-2 weeks" = -14L, "-2 days" = -2L),
  integer(1),
  USE.NAMES = FALSE
)


expect_equal(
  runner(1:100, k = 5, lag = lag_date, idx = idx_date, f = function(x) x),
  runner(1:100, k = 5, lag = lag_int, idx = idx_date, f = function(x) x)
)

#runner with df -------

#runner with matrix -------
data <- matrix(data = runif(100, 0, 1), nrow = 20, ncol = 5)
res <- runner(
  x = data,
  f = function(x) {
    tryCatch(
      cor(x),
      error = function(e) NA
    )
  })

expected <- sapply(
  X = 1:20,
  FUN = function(i) {
    tryCatch(
      cor(data[1:i, , drop = FALSE]),
      error = function(e) NA
    )
  }, simplify = TRUE)

expect_identical(res, expected)

#runner with xts -------
xts_object <- structure(
  .Data = matrix(
    data = runif(100, 0, 1),
    nrow = 20,
    ncol = 5,
    dimnames = list(NULL, c("open", "close", "high", "low", "volume"))
  ),
  index = structure(
    .Data = as.vector(seq.POSIXt(Sys.time(), by = "1 month", length.out = 20)),
    tclass = c("POSIXct", "POSIXt"),
    tzone = ""
  ),
  class = c("xts", "zoo")
)

res <- runner(
  x = xts_object,
  f = function(x) {
    tryCatch(
      x,
      error = function(e) NA
    )
  })

expected <- sapply(
  X = 1:20,
  FUN = function(i) {
    tryCatch(
      xts_object[1:i, , drop = FALSE],
      error = function(e) NA
    )
  },
  simplify = TRUE
)

expect_identical(res, expected)

#Parallel -------

#Errors -------
expect_error(runner(x = letters[1:5], f = ""))

expect_error(runner(list(1:10), k = 5, f = mean), "Invalid \\'x\\' type")

expect_error(runner(1:10, k = -5, f = mean),
             "k can't be negative")

expect_error(runner(1:10, k = (1:9), f = mean),
             "length of k and length of x differs")
expect_error(runner(1:10, k = c(NA, 1:9), f = mean),
             "Function doesn't accept NA values in k vector")

expect_error(runner(1:10, lag = (1:9), f = mean),
             "length of lag and length of x differs")
expect_error(runner(x1, k = k, lag = integer(0), idx = idx, f = mean),
             "length of lag should not be zero")
expect_error(runner(1:10, lag = c(NA, 1:9), f = mean),
             "Function doesn't accept NA values in lag vector")

expect_error(runner(1:10, idx = (1:9), f = mean),
             "length of idx and length of x differs")
expect_error(runner(1:10, idx = c(NA, 1:9), f = mean),
             "Function doesn't accept NA values in idx vector")
expect_error(runner(1:10, idx = sample(1:10), f = mean),
             "idx have to be in ascending order")


expect_error(
  runner(1:10, k = rep(5, 10), idx = 1:10, at = c(4, 5), f = mean),
  "length\\(k\\) should be 1 or equal to"
)

expect_error(
  runner(1:10, lag = rep(5, 10), idx = 1:10, at = c(4, 5), f = mean),
  "length\\(lag\\) should be 1 or equal to"
)

