# k by ------
idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)
attr(idx, "tzone") <- ""


new_k <- runner:::k_by(k = "2 months", idx = idx, param = "k")
expected <- as.POSIXct(
  c("2018-11-01 03:02:01", "2018-12-01 03:02:01", "2019-01-01 03:02:01",
    "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
    "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
    "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
    "2019-11-01 03:02:01")
)

tinytest::expect_equal(idx - new_k, expected)

idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)
attr(idx, "tzone") <- ""

new_k <- runner:::k_by(k = "month", idx = idx, param = "k")
expected <- as.POSIXct(
  c("2018-12-01 03:02:01", "2019-01-01 03:02:01",
    "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
    "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
    "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
    "2019-11-01 03:02:01", "2019-12-01 03:02:01")
)

tinytest::expect_equal(idx - new_k, expected)


idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)
attr(idx, "tzone") <- ""
new_k <- runner:::k_by(k = "month", idx = idx, param = "lag")
expected <- as.POSIXct(
  c("2018-12-01 03:02:01", "2019-01-01 03:02:01",
    "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
    "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
    "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
    "2019-11-01 03:02:01", "2019-12-01 03:02:01")
)

tinytest::expect_equal(idx - new_k, expected)


idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)
attr(idx, "tzone") <- ""
new_k <- runner:::k_by(k = "-1 month", idx = idx, param = "lag")
expected <- as.POSIXct(
  c("2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
    "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
    "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
    "2019-11-01 03:02:01", "2019-12-01 03:02:01", "2020-01-01 03:02:01",
    "2020-02-01 03:02:01")
)

tinytest::expect_equal(idx - new_k, expected)


idx <- seq(
  as.POSIXct("2020-01-01 03:02:01"),
  as.POSIXct("2020-02-01 03:02:01"),
  by = "month"
)
attr(idx, "tzone") <- ""
new_k <- runner:::k_by(k = c("-1 month", "1 week"), idx = idx, param = "lag")
expected <- as.POSIXct(
  c("2020-02-01 03:02:01", "2020-01-25 03:02:01")
)

tinytest::expect_equal(idx - new_k, expected)


tinytest::expect_equal(
  runner:::k_by(k = "1 weeks", idx = idx, param = "lag"),
  runner:::k_by(k = as.difftime(1, unit = "weeks"), idx = idx, param = "lag")
)


tinytest::expect_equal(
  runner:::k_by(k = "2 weeks", idx = idx, param = "k"),
  runner:::k_by(k = as.difftime(2, units = "weeks"), idx = idx, param = "k")
)


tinytest::expect_equal(
  runner:::k_by(k = as.difftime(-2, units = "weeks"), idx = idx, param = "lag"),
  runner:::k_by(k = "-2 weeks", idx = idx, param = "lag"),
)


tinytest::expect_equal(
  runner:::k_by(k = as.difftime(2, units = "weeks"), idx = idx, param = "lag"),
  runner:::k_by(k = "2 weeks", idx = idx, param = "lag"),
)


idx <- seq(as.POSIXct("2019-01-01 03:02:01"),
           as.POSIXct("2020-01-01 03:02:01"),
           by = "month")
tinytest::expect_error(
  runner:::k_by(k = "-1 month", idx = idx, param = "k"),
  "k can't be negative"
)


tinytest::expect_error(
  runner:::k_by(k = as.difftime(-2, units = "weeks"), idx = idx, param = "k"),
  "`k` can't be negative"
)

# reformat k -------
tinytest::expect_equal(
  runner:::reformat_k("day", only_positive = TRUE),
  "-1 days"
)

tinytest::expect_equal(
  runner:::reformat_k("2 days", only_positive = TRUE),
  "-2 days"
)

tinytest::expect_equal(
  runner:::reformat_k(c("2 days", "2 months"), only_positive = TRUE),
  c("-2 days", "-2 months")
)


tinytest::expect_equal(
  runner:::reformat_k("2 days", only_positive = FALSE),
  "-2 days"
)

tinytest::expect_equal(
  runner:::reformat_k(c("day", "-2 days", "2 days"), only_positive = FALSE),
  c("-1 days", "2 days", "-2 days")
)

tinytest::expect_equal(
  runner:::reformat_k("2 days", only_positive = FALSE),
  "-2 days"
)


tinytest::expect_error(
  runner:::reformat_k("-2 days", only_positive = TRUE),
  "k can't be negative"
)


tinytest::expect_error(
  runner:::reformat_k(c("2 days", "-2 days"), only_positive = TRUE),
  "k can't be negative"
)

# runner:::seq_at ------
idx <- seq(
  as.POSIXct("2019-01-01 03:02:01"),
  as.POSIXct("2020-01-01 03:02:01"),
  by = "month"
)

out <- runner:::seq_at(at = "2 months", idx = idx)
expected <- as.POSIXct(
  c("2019-01-01 03:02:01", "2019-03-01 03:02:01", "2019-05-01 03:02:01",
    "2019-07-01 03:02:01", "2019-09-01 03:02:01", "2019-11-01 03:02:01",
    "2020-01-01 03:02:01")
)
attr(expected, "tzone") <- ""
attr(out, "tzone") <- ""
tinytest::expect_equal(out, expected)

out <- runner:::seq_at(at = "-2 months", idx = idx)
attr(out, "tzone") <- ""
tinytest::expect_equal(out, rev(expected))

tinytest::expect_error(
  runner:::seq_at(at = "-2 months", idx = integer(0)),
  "`idx` can't be empty while specifying `at`"
)


tinytest::expect_identical(
  runner:::seq_at(at = "2 weeks", idx = idx),
  runner:::seq_at(at = as.difftime(2, units = "weeks"), idx = idx)
)

tinytest::expect_identical(
  runner:::seq_at(at = "-2 weeks", idx = idx),
  runner:::seq_at(at = as.difftime(-2, units = "weeks"), idx = idx)
)
