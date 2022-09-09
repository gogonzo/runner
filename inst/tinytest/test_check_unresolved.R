data <- data.frame(
  index = cumsum(sample(0:3, 20, replace = TRUE)),
  group1 = sample(c("a", "b"), 20, replace = TRUE),
  group2 = sample(c("A", "B"), 20, replace = TRUE),
  x = 1:20,
  k = c(rep(2, 10), rep(5, 10)),
  lag = sample(0:3, 20, replace = TRUE)
)

f1 <- function(x, k) {
  runner:::.check_unresolved_difftime(x, k)
}

expect_identical(f1(data, NULL), NULL, info = "k can be empty")
expect_identical(f1(data, "k"), NULL, info = "k can be a column name")
expect_error(f1(data, "xxx"), "`k` is invalid", info = "k can't be a wrong column name")
expect_identical(f1(data, "2 days"), NULL, info = "k can be a single datetime ")
expect_identical(f1(data, rep("2 days", 20)), NULL, info = "k can be a datetime of length=nrow(x)")
expect_error(f1(data, rep("2 days", 10)), "`k` is invalid", info = "length of k should be either 1 or nrow(x)")
expect_error(f1(data, NA), "`k` is invalid", info = "k cant be NA")
expect_error(f1(data, "group1"), "`k` is invalid", info = "k cant be a name of the column which isn't valid")

f2 <- function(x, idx) {
  runner:::.check_unresolved_index(x, idx)
}

expect_identical(f2(data, NULL), NULL, info = "idx can be empty")
expect_identical(f2(data, 1:20), NULL, info = "idx can be a vector of length == nrow(x)")
expect_identical(f2(data, "index"), NULL, info = "idx can be a column name")
expect_error(f2(data, "xxx"), "`idx` is invalid", info = "idx can't be a wrong column name")
expect_error(f2(data, 1), "`idx` is invalid", info = "idx can't be a single value")
expect_error(f2(data, 1:10), "`idx` is invalid", info = "length(idx) == nrow(x) only")
expect_error(f2(data, NA), "`idx` is invalid", info = "idx cant be NA")
expect_error(f2(data, "group1"), "`idx` is invalid", info = "idx cant be a name of the column which isn't valid")

f3 <- function(x, at) {
  runner:::.check_unresolved_at(x, at)
}

expect_identical(f3(data, NULL), NULL, info = "at can be empty")
expect_identical(f3(data, 1:20), NULL, info = "idx can be a vector of length == nrow(x)")
expect_identical(f3(data, 1), NULL, info = "at can be a single value")
expect_identical(f3(data, 1:100), NULL, info = "at can be of any length")
expect_identical(f3(data, "index"), NULL, info = "at can be a column name")
expect_error(f3(data, "xxx"), "`at` is invalid", info = "at can't be a wrong column name")
expect_error(f3(data, NA), "`at` is invalid", info = "at cant be NA")
expect_error(f3(data, "group1"), "`at` is invalid", info = "at cant be a name of the column which isn't valid")
