library(bench)
library(runner)
library(runner2)
library(slider)

set.seed(42)

cat("=== Package versions ===\n")
cat("runner (new):", as.character(packageVersion("runner")), "\n")
cat("runner2 (old):", as.character(packageVersion("runner2")), "\n")
cat("slider:", as.character(packageVersion("slider")), "\n\n")

# --- Benchmark 1: Simple cumulative mean on vector ---
cat("=== 1. Cumulative mean (no k, no idx) ===\n")
x <- rnorm(10000)
res <- mark(
  runner_new  = runner::runner(x, f = mean),
  runner_old  = runner2::runner(x, f = mean),
  slider      = slider::slide_dbl(x, mean, .before = Inf),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 2: Fixed window k=100, mean ---
cat("\n=== 2. Fixed window k=100, mean ===\n")
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 100),
  runner_old  = runner2::runner(x, f = mean, k = 100),
  slider      = slider::slide_dbl(x, mean, .before = 99),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 3: Fixed window k=10, mean, with lag ---
cat("\n=== 3. Fixed window k=10, lag=2, mean ===\n")
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 10, lag = 2),
  runner_old  = runner2::runner(x, f = mean, k = 10, lag = 2),
  slider      = slider::slide_dbl(x, mean, .before = 11, .after = -2),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 4: Varying window size ---
cat("\n=== 4. Varying window size (k vector) ===\n")
k_vec <- sample(1:50, 10000, replace = TRUE)
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = k_vec),
  runner_old  = runner2::runner(x, f = mean, k = k_vec),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 5: With idx (date-based windows) ---
cat("\n=== 5. Date-based windows (idx + k=50) ===\n")
idx <- cumsum(sample(1:3, 10000, replace = TRUE))
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 50, idx = idx),
  runner_old  = runner2::runner(x, f = mean, k = 50, idx = idx),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 6: With `at` (output at specific indices) ---
cat("\n=== 6. Output at specific indices (at, k=50) ===\n")
at_vec <- sort(sample(1:10000, 500))
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 50, at = at_vec),
  runner_old  = runner2::runner(x, f = mean, k = 50, at = at_vec),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 7: na_pad = TRUE ---
cat("\n=== 7. na_pad=TRUE, k=100 ===\n")
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 100, na_pad = TRUE),
  runner_old  = runner2::runner(x, f = mean, k = 100, na_pad = TRUE),
  slider      = slider::slide_dbl(x, mean, .before = 99, .complete = TRUE),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 8: data.frame input ---
cat("\n=== 8. data.frame input, k=50 ===\n")
df <- data.frame(a = rnorm(5000), b = rnorm(5000))
res <- mark(
  runner_new  = runner::runner(df, f = function(x) cor(x$a, x$b), k = 50),
  runner_old  = runner2::runner(df, f = function(x) cor(x$a, x$b), k = 50),
  min_iterations = 3,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 9: Custom function with extra args (... passing) ---
cat("\n=== 9. Custom function with extra args ===\n")
my_fun <- function(x, trim) mean(x, trim = trim)
res <- mark(
  runner_new  = runner::runner(x, f = my_fun, k = 50, trim = 0.1),
  runner_old  = runner2::runner(x, f = my_fun, k = 50, trim = 0.1),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 10: character vector input ---
cat("\n=== 10. Character vector, k=20 ===\n")
x_chr <- sample(letters, 5000, replace = TRUE)
res <- mark(
  runner_new  = runner::runner(x_chr, f = function(x) paste(x, collapse = ""), k = 20),
  runner_old  = runner2::runner(x_chr, f = function(x) paste(x, collapse = ""), k = 20),
  min_iterations = 3,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 11: Larger dataset, simple function ---
cat("\n=== 11. Large dataset (50k), k=10, sum ===\n")
x_large <- rnorm(50000)
res <- mark(
  runner_new  = runner::runner(x_large, f = sum, k = 10),
  runner_old  = runner2::runner(x_large, f = sum, k = 10),
  slider      = slider::slide_dbl(x_large, sum, .before = 9),
  min_iterations = 3,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])

# --- Benchmark 12: idx + at combined ---
cat("\n=== 12. idx + at combined, k=30 ===\n")
idx2 <- cumsum(sample(1:4, 10000, replace = TRUE))
at2 <- sort(sample(idx2, 200))
res <- mark(
  runner_new  = runner::runner(x, f = mean, k = 30, idx = idx2, at = at2),
  runner_old  = runner2::runner(x, f = mean, k = 30, idx = idx2, at = at2),
  min_iterations = 5,
  check = FALSE
)
print(res[, c("expression", "min", "median", "mem_alloc", "n_itr")])
