data <- data.frame(
  index = cumsum(sample(0:3, 20, replace = TRUE)),
  group1 = sample(c("a", "b"), 20, replace = TRUE),
  group2 = sample(c("A", "B"), 20, replace = TRUE),
  x = 1:20,
  k = c(rep(2, 10), rep(5, 10)),
  lag = sample(0:3, 20, replace = TRUE)
)

f1 <- function(x, k) {
  .check_unresolved_difftime(x, k)
}

expect_identical(f1(data, "k"), NULL, info = "k can be a column name")
expect_error(f1(data, "xxx"), NULL, info = "k can't be a wrong column name")
expect_identical(f1(data, "2 days"), NULL, info = "k can be a single datetime ")

expect_identical(f1(data, rep("2 days", 20)), NULL, info = "k can be a datetime of length=nrow(x)")
expect_error(f1(data, rep("2 days", 10)), NULL, info = "length of k should be wither 1 or nrow(x)")
