set.seed(1)
# create minute data -----
minutes <- seq(
  as.POSIXct("2007-01-01 00:00:00"),
  as.POSIXct(Sys.Date()),
  by = "min"
)
minutes <- sort(sample(minutes, 100000, replace = TRUE))
sum(duplicated(minutes))

dummy_min <- data.frame(
  min = minutes,
  val1 = rnorm(100000)
)

dummy_min$val2 <- 2 * dummy_min$val1 + rnorm(100000)
dummy_min$val3 <- -1.5 * dummy_min$val1 + 2 * rnorm(100000)
dummy_min$val4 <- 0.7 * dummy_min$val2 + rnorm(100000)

dummy_min$val1 <- cumsum(dummy_min$val1)
dummy_min$val2 <- cumsum(dummy_min$val2)
dummy_min$val3 <- cumsum(dummy_min$val3)
dummy_min$val4 <- cumsum(dummy_min$val4)


# create hourly data -----
dummy_hour <- dummy_min[sample(seq_len(nrow(dummy_min)), 10000, replace = TRUE), ]
dummy_hour <- dummy_hour[order(dummy_hour$min),]
dummy_hour$min <- lubridate::round_date(dummy_hour$min, unit = "hour")
colnames(dummy_hour)[1] <- "hour"
dummy_hour <- dummy_hour[, c("hour", "val2")]

# create daily data ----
dummy_day <- dummy_min[sample(seq_len(nrow(dummy_min)), 10000, replace = TRUE), ]
dummy_day <- dummy_day[order(dummy_day$min),]
dummy_day$min <- lubridate::round_date(dummy_day$min, unit = "day")
colnames(dummy_day)[1] <- "day"
dummy_day <- dummy_day[, c("day", "val3", "val4")]


library(ggplot2)
ggplot(dummy_min, aes(x = min, y = val1)) +
  geom_line() +
  geom_line(data = dummy_hour, aes(x = hour, y = val2), color = "red") +
  geom_line(data = dummy_day, aes(x = day, y = val3), color = "blue") +
  geom_line(data = dummy_day, aes(x = day, y = val4), color = "blue")


usethis::use_data(
  dummy_min,
  dummy_hour,
  dummy_day,
  overwrite = TRUE
)
