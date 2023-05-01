context("act_tbl S3 object")
library(activatr)

test_that("validation works", {
  expect_error(act_tbl(tibble::tribble(
    ~foo, "bar"
  )))
})

test_that("summary works", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:29:00"), 0.05, 0.05, 500,
    lubridate::ymd_hms("2019-01-01 1:00:01"), 0.1, 0.1, 200
  ))
  s <- summary(df)
  expect_true(tibble::is_tibble(s))
  expect_equal(nrow(s), 1)
  expect_equal(as.integer(s$Distance), 9)

  expect_equal(s$Date, lubridate::ymd_hms("2019-01-01 0:00:00"))
  expect_is(s$Time, "Duration")
  expect_equal(as.integer(s$Time), 3601)

  expect_is(s$AvgPace, "Duration")
  expect_equal(as.integer(s$AvgPace), 369)
  expect_is(s$MaxPace, "Duration")
  expect_equal(as.integer(s$MaxPace), 356)

  expect_equal(as.integer(s$AvgElev), 765)
  expect_equal(as.integer(s$ElevGain), 1640)
  expect_equal(as.integer(s$ElevLoss), 984)
})

test_that("summary allows use of full to fill in missing columns", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:30"), 0.5, 0.5,
    lubridate::ymd_hms("2019-01-01 0:01:01"), 1, 1
  ))
  s <- summary(df)
  expect_false("AvgElev" %in% colnames(s))
  expect_false("ElevGain" %in% colnames(s))
  expect_false("ElevLoss" %in% colnames(s))

  s2 <- summary(df, full = TRUE)
  expect_true("AvgElev" %in% colnames(s2))
  expect_true("ElevGain" %in% colnames(s2))
  expect_true("ElevLoss" %in% colnames(s2))
  expect_equal(s2$AvgElev, NA)
  expect_equal(s2$ElevGain, NA)
  expect_equal(s2$ElevLoss, NA)
})

test_that("summary allows for metric system", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:29:00"), 0.05, 0.05, 500,
    lubridate::ymd_hms("2019-01-01 1:00:01"), 0.1, 0.1, 200
  ))
  s <- summary(df, units = "metric")
  expect_true(tibble::is_tibble(s))
  expect_equal(nrow(s), 1)
  expect_equal(as.integer(s$Distance), 15)

  expect_equal(s$Date, lubridate::ymd_hms("2019-01-01 0:00:00"))
  expect_is(s$Time, "Duration")
  expect_equal(as.integer(s$Time), 3601)

  expect_is(s$AvgPace, "Duration")
  expect_equal(as.integer(s$AvgPace), 229)
  expect_is(s$MaxPace, "Duration")
  expect_equal(as.integer(s$MaxPace), 221)

  expect_equal(as.integer(s$AvgElev), 233)
  expect_equal(as.integer(s$ElevGain), 500)
  expect_equal(as.integer(s$ElevLoss), 300)
})

test_that("summary includes hr when provided", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~hr,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 120,
    lubridate::ymd_hms("2019-01-01 0:00:30"), 0.5, 0.5, 130,
    lubridate::ymd_hms("2019-01-01 0:01:01"), 1, 1, 140,
  ))
  s <- summary(df)
  expect_equal(s$AvgHR, 130)
  expect_equal(s$MaxHR, 140)
})

test_that("summary includes cad when provided", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~cad,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 70,
    lubridate::ymd_hms("2019-01-01 0:00:30"), 0.5, 0.5, 80,
    lubridate::ymd_hms("2019-01-01 0:01:01"), 1, 1, 90
  ))
  s <- summary(df)
  expect_equal(s$AvgCadence, 80)
  expect_equal(s$MaxCadence, 90)
})

test_that("summary includes cad when provided", {
  result <- tibble::tribble(
    ~time, ~lat, ~lon, ~cad,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 70,
    lubridate::ymd_hms("2019-01-01 0:00:30"), 0.5, 0.5, 80,
    lubridate::ymd_hms("2019-01-01 0:01:01"), 1, 1, 90
  )
  attr(result, "title") <- "The Title"
  df <- act_tbl(result)
  s <- summary(df)
  expect_equal(s$Title, "The Title")
})

test_that("release questions are non-empty", {
  expect_gt(length(activatr:::release_questions()), 0)
})
