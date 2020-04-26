context("external compatibility")

test_that("dplyr::group_by - dplyr keeps attributes", {
  data <- data.frame(
    index = cumsum(sample(0:3, 20, replace = TRUE)),
    group1 = sample(c("a", "b"), 20, replace = TRUE),
    group2 = sample(c("A", "B"), 20, replace = TRUE),
    x = 1:20,
    k = c(rep(2, 10), rep(5, 10)),
    lag = sample(0:3, 20, replace = TRUE)
  )

  #
  grouped_index <- data %>%
    dplyr::group_by(group1, group2) %>%
    run_by(idx = "index")

  expect_identical(
    dplyr::groups(grouped_index),
    list(as.name("group1"), as.name("group2"))
  )

  expect_identical(
    attr(grouped_index, "idx"),
    "index"
  )

  grouped_index <- data %>%
    run_by(idx = "index") %>%
    dplyr::group_by(group1, group2)

  expect_identical(
    dplyr::groups(grouped_index),
    list(as.name("group1"), as.name("group2"))
  )

  expect_identical(
    attr(grouped_index, "idx"),
    "index"
  )


  res <- grouped_index %>%
    dplyr::mutate(
      x = runner(
        .,
        f = function(x) {
          unique(paste(x$group1, x$group2))
        }
      )
    )
  expect_identical(
    res$x,
    paste(grouped_index$group1, grouped_index$group2)
  )
})

test_that("correct grouping results", {

  data <- data.frame(
    index = c(2, 5, 5, 7, 10, 12, 12, 12, 15, 16, 19,
              20, 20, 21, 23, 23, 25, 27, 27, 28),
    group1 = rep(c("a", "b"), each = 20),
    group2 = rep(c("A", "B"), each = 20),
    x = 1:20
  )

  #
  grouped_dplyr <- data %>%
    dplyr::group_by(group1, group2) %>%
    run_by(idx = "index") %>%
    dplyr::mutate(
      xx = runner(x, f = mean)
    )


  expect_equal(
    grouped_dplyr$xx,
    c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5,
      8.0, 8.5, 9.0, 9.5, 10.0, 10.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5,
      5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0, 10.5)
  )

})
