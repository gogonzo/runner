context("timeops")


test_that("k by", {
  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  new_k <- k_by(k = "2 months", idx = idx, param = "k")
  expected <- as.POSIXct(
    c("2018-11-01 03:02:01", "2018-12-01 03:02:01", "2019-01-01 03:02:01",
      "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
      "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
      "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
      "2019-11-01 03:02:01")
  )

  expect_equal(idx - new_k, expected)

  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  new_k <- k_by(k = "month", idx = idx, param = "k")
  expected <- as.POSIXct(
    c("2018-12-01 03:02:01", "2019-01-01 03:02:01",
      "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
      "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
      "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
      "2019-11-01 03:02:01", "2019-12-01 03:02:01")
  )

  expect_equal(idx - new_k, expected)


  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  new_k <- k_by(k = "month", idx = idx, param = "lag")
  expected <- as.POSIXct(
    c("2018-12-01 03:02:01", "2019-01-01 03:02:01",
      "2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
      "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
      "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
      "2019-11-01 03:02:01", "2019-12-01 03:02:01")
  )

  expect_equal(idx - new_k, expected)


  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  new_k <- k_by(k = "-1 month", idx = idx, param = "lag")
  expected <- as.POSIXct(
    c("2019-02-01 03:02:01", "2019-03-01 03:02:01", "2019-04-01 03:02:01",
      "2019-05-01 03:02:01", "2019-06-01 03:02:01", "2019-07-01 03:02:01",
      "2019-08-01 03:02:01", "2019-09-01 03:02:01", "2019-10-01 03:02:01",
      "2019-11-01 03:02:01", "2019-12-01 03:02:01", "2020-01-01 03:02:01",
      "2020-02-01 03:02:01")
  )

  expect_equal(idx - new_k, expected)


  idx <- seq(as.POSIXct("2020-01-01 03:02:01"), as.POSIXct("2020-02-01 03:02:01"), by = "month")
  new_k <- k_by(k = c("-1 month", "1 week"), idx = idx, param = "lag")
  expected <- as.POSIXct(
    c("2020-02-01 03:02:01", "2020-01-25 03:02:01")
  )

  expect_equal(idx - new_k, expected)


  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  expect_error(
    k_by(k = "-1 month", idx = idx, param = "k"),
    "k can't be negative"
  )

  expect_error(
    k_by(k = "1 month", idx = integer(0), param = "lag"),
    "`idx` can't be empty"
  )
})

test_that("reformat k", {
  expect_equal(
    reformat_k("day", only_positive = TRUE),
    "-1 days"
  )

  expect_equal(
    reformat_k("2 days", only_positive = TRUE),
    "-2 days"
  )

  expect_equal(
    reformat_k(c("2 days", "2 months"), only_positive = TRUE),
    c("-2 days", "-2 months")
  )


  expect_equal(
    reformat_k("2 days", only_positive = FALSE),
    "-2 days"
  )

  expect_equal(
    reformat_k(c("day", "-2 days", "2 days"), only_positive = FALSE),
    c("-1 days", "2 days", "-2 days")
  )


  expect_error(
    reformat_k("-2 days", only_positive = TRUE),
    "k can't be negative"
  )


  expect_error(
    reformat_k(c("2 days","-2 days"), only_positive = TRUE),
    "k can't be negative"
  )

})



test_that("seq_by", {
  idx <- seq(as.POSIXct("2019-01-01 03:02:01"), as.POSIXct("2020-01-01 03:02:01"), by = "month")
  out <- seq_by(at = "2 months", idx = idx)
  expected <- as.POSIXct(
    c("2019-01-01 03:02:01", "2019-03-01 03:02:01", "2019-05-01 03:02:01",
      "2019-07-01 03:02:01", "2019-09-01 03:02:01", "2019-11-01 03:02:01",
      "2020-01-01 03:02:01")
  )
  expect_equal(out, expected)

  out <- seq_by(at = "-2 months", idx = idx)
  expect_equal(out, rev(expected))


  expect_error(
    seq_by(at = "-2 months", idx = integer(0)),
    "`idx` can't be empty while specifying at"
  )

  expect_error(
    seq_by(at = "-2 months", idx = 1:2),
    "To specify at as time interval character `idx` can't be empty"
  )
})
