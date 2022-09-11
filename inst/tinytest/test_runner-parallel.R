data <- data.frame(
  a = runif(100),
  b = runif(100),
  idx = cumsum(sample(rpois(100, 5)))
)

# vector
cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner::runner(
    x = data$a,
    k = 10,
    f = sum,
    idx = data$idx,
    simplify = TRUE
  ),
  runner::runner(
    x = data$a,
    k = 10,
    f = sum,
    idx = data$idx,
    cl = cl,
    simplify = TRUE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "numeric"))

cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner::runner(
    x = data$a,
    k = 10,
    f = sum,
    idx = data$idx,
    simplify = FALSE
  ),
  runner::runner(
    x = data$a,
    k = 10,
    f = sum,
    idx = data$idx,
    cl = cl,
    simplify = FALSE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "list"))

# data.frame
cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner(
    x = data,
    k = 10,
    f = sum,
    idx = "idx",
    cl = cl,
    simplify = TRUE
  ),
  runner(
    x = data,
    k = 10,
    f = sum,
    idx = "idx",
    simplify = TRUE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "numeric"))

cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner(
    x = data,
    k = 10,
    f = sum,
    idx = "idx",
    cl = cl,
    simplify = FALSE
  ),
  runner(
    x = data,
    k = 10,
    f = sum,
    idx = "idx",
    simplify = FALSE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "list"))

# matrix
cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner(
    x = matrix(seq_len(100), nrow = 20, ncol = 5),
    f = sum,
    k = 10,
    idx = 1:20,
    cl = cl,
    simplify = TRUE
  ),
  runner(
    x = matrix(seq_len(100), nrow = 20, ncol = 5),
    f = sum,
    k = 10,
    idx = 1:20,
    simplify = TRUE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "integer"))

cl <- parallel::makeCluster(1)
expect_identical(
  res <- runner(
    x = matrix(1:100, nrow = 20, ncol = 5),
    f = sum,
    k = 10,
    idx = 1:20,
    cl = cl,
    simplify = FALSE
  ),
  runner(
    x = matrix(1:100, nrow = 20, ncol = 5),
    f = sum,
    k = 10,
    idx = 1:20,
    simplify = FALSE
  )
)
parallel::stopCluster(cl)
expect_true(is(res, "list"))
