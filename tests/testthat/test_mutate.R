context("Mutating Activatr DFs")
library(activatr)

test_that("mutate_with_distance works on a trivial data set", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  ))
  distance_df <- mutate_with_distance(df)
  expect_equal(as.integer(distance_df$distance), c(NA, 156899))
})

test_that("mutate_with_distance works correctly accounts for 3D", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  ))
  distance_df <- mutate_with_distance(df, method = "3D")
  expect_equal(as.integer(distance_df$distance), c(NA, 157217))
})

test_that("mutate_with_distance works for 2D if missing ele", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1
  ))
  distance_df <- mutate_with_distance(df, method = "2D")
  expect_equal(as.integer(distance_df$distance), c(NA, 156899))
})

test_that("mutate_with_distance errors for 3D if missing ele", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1
  ))
  expect_error(mutate_with_distance(df, method = "3D"))
})

test_that("mutate_with_distance handles no movements, so speed is 0", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000,
    lubridate::ymd_hms("2019-01-01 0:00:02"), 1, 1, 10000
  ))
  distance_df <- mutate_with_distance(df)
  expect_equal(as.integer(distance_df$distance), c(NA, 156899, 0))
})

test_that("mutate_with_distance respects lead and lag", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:02"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:03"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:04"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:05"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:06"), 4, 4, 0,
    lubridate::ymd_hms("2019-01-01 0:00:07"), 4, 4, 0,
    lubridate::ymd_hms("2019-01-01 0:00:08"), 4, 4, 0
  ))

  # With only 1 in each direction, the middle point in each bunch has 0 speed
  distance_df <- mutate_with_distance(df, lead = 1, lag = 1)
  expect_equal(
    as.integer(distance_df$distance),
    c(NA, 0, 313775, 313775, 0, 313588, 313588, 0, NA)
  )

  # With 2 in each direction, we get a smoother speed
  distance_df_wider <- mutate_with_distance(df, lead = 2, lag = 2)
  expect_equal(
    as.integer(distance_df_wider$distance),
    c(NA, NA, 313775, 313775, 627363, 313588, 313588, NA, NA)
  )
})

test_that("mutate_with_speed works on a trivial data set", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  ))
  speed_df <- mutate_with_speed(df)
  expect_equal(as.integer(speed_df$speed), c(NA, 156899))
})

test_that("mutate_with_speed works correctly accounts for 3D", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  ))
  speed_df <- mutate_with_speed(df, method = "3D")
  expect_equal(as.integer(speed_df$speed), c(NA, 157217))
})

test_that("mutate_with_speed errors if given just lat and lon", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    0, 0,
    1, 1
  ))
  expect_error(mutate_with_speed(df))
})

test_that("mutate_with_speed works for 2D if missing ele", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1
  ))
  speed_df <- mutate_with_speed(df, method = "2D")
  expect_equal(as.integer(speed_df$speed), c(NA, 156899))
})

test_that("mutate_with_speed errors for 3D if missing ele", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1
  ))
  expect_error(mutate_with_speed(df, method = "3D"))
})

test_that("mutate_with_speed handles no movements, so speed is 0", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000,
    lubridate::ymd_hms("2019-01-01 0:00:02"), 1, 1, 10000
  ))
  speed_df <- mutate_with_speed(df)
  expect_equal(as.integer(speed_df$speed), c(NA, 156899, 0))
})

test_that("mutate_with_speed respects lead and lag", {
  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:00"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:02"), 0, 0, 0,
    lubridate::ymd_hms("2019-01-01 0:00:03"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:04"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:05"), 2, 2, 0,
    lubridate::ymd_hms("2019-01-01 0:00:06"), 4, 4, 0,
    lubridate::ymd_hms("2019-01-01 0:00:07"), 4, 4, 0,
    lubridate::ymd_hms("2019-01-01 0:00:08"), 4, 4, 0
  ))

  # With only 1 in each direction, the middle point in each bunch has 0 speed
  speed_df <- mutate_with_speed(df, lead = 1, lag = 1)
  expect_equal(
    as.integer(speed_df$speed),
    c(NA, 0, 156887, 156887, 0, 156794, 156794, 0, NA)
  )

  # With 2 in each direction, we get a smoother speed
  speed_df_wider <- mutate_with_speed(df, lead = 2, lag = 2)
  expect_equal(
    as.integer(speed_df_wider$speed),
    c(NA, NA, 78443, 78443, 156840, 78397, 78397, NA, NA)
  )
})

test_that("meters_to_miles is correct", {
  pace <- meters_to_miles(5000)
  expect_equal(as.integer(pace * 1000), 3106)
})

test_that("speed_to_mile_pace is correct", {
  pace <- speed_to_mile_pace(1)
  expect_is(pace, "Duration")
  expect_equal(as.integer(pace), 1609)
})

test_that("speed_to_mile_pace returns NA for a speed of 0", {
  pace <- speed_to_mile_pace(0)
  expect_is(pace, "Duration")
  expect_true(is.na(pace))
})

test_that("pace_formatter is correct for normal values", {
  pace <- dseconds(570)
  expect_equal(pace_formatter(pace), "9:30")
})

test_that("pace_formatter is correct for less than minute values", {
  pace <- dseconds(30)
  expect_equal(pace_formatter(pace), "0:30")
})

test_that("pace_formatter is correct for more than hour values", {
  pace <- dseconds(3630)
  expect_equal(pace_formatter(pace), "60:30")
})
