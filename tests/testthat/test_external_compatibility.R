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
