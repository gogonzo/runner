library(dplyr)

# dplyr::group_by - dplyr keeps attributes -----
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

expect_true(is(grouped_index, "grouped_df"))
expect_true(is(grouped_index, "data.frame"))

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

data <- data.frame(
  index = c(2, 5, 5, 7, 10, 12, 12, 12, 15, 16, 19,
            20, 20, 21, 23, 23, 25, 27, 27, 28),
  group1 = rep(c("a", "b"), each = 20),
  group2 = rep(c("A", "B"), each = 20),
  x = 1:20
)

# single column in mutate is grouped ------
res <- data %>%
  group_by(group1) %>%
  mutate(
    m = length(x)
  )

expect_true(
  all(res$m == 20)
)

# running on grouped_df index set -----
res <- data %>%
  dplyr::group_by(group1, group2) %>%
  run_by(idx = "index") %>%
  dplyr::mutate(
    xx = runner(
      .,
      f = function(df) {
        paste(df$x, collapse = " ")
      }
    )
  )

expected <- unlist(
  use.names = FALSE,
  tapply(
    data$x,
    paste(data$group1, data$group2),
    runner,
    f = paste,
    collapse = " "
  )
)

expect_identical(
  res$xx,
  expected
)


# running on columns from grouped_df k without idx -----
grouped_dplyr <- data %>%
  dplyr::group_by(group1, group2) %>%
  dplyr::mutate(
    xx = runner(
      x = .,
      f = function(df) {
        paste(df$x, collapse = " ")
      },
      k = 2
    )
  )

expected <- unlist(
  use.names = FALSE,
  tapply(
    data$x,
    paste(data$group1, data$group2),
    runner,
    f = paste,
    collapse = " ",
    k = 2
  )
)

expect_equal(
  grouped_dplyr$xx,
  expected
)

# running on columns from grouped_df k with idx -----
grouped_dplyr <- data %>%
  dplyr::group_by(group1, group2) %>%
  dplyr::mutate(
    xx = runner(
      x = .,
      f = function(df) {
        paste(df$x, collapse = " ")
      },
      k = 2,
      idx = index
    )
  )

grouped_dplyr2 <- data %>%
  dplyr::group_by(group1, group2) %>%
  run_by(idx = "index") %>%
  dplyr::mutate(
    xx = runner(
      x = .,
      f = function(df) {
        paste(df$x, collapse = " ")
      },
      k = 2
    )
  )


expected <- unlist(
  use.names = FALSE,
  tapply(
    1:40,
    paste(data$group1, data$group2),
    function(idx) {
      runner(
        data$x[idx],
        f = paste,
        collapse = " ",
        k = 2,
        idx = data$index[idx]
      )
    }
  )
)

expect_equal(
  grouped_dplyr$xx,
  expected
)

expect_equal(
  grouped_dplyr2$xx,
  expected
)
