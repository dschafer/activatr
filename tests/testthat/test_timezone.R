context("Updating Activatr DFs with correct time zones")
library(activatr)
library(lubridate)
library(mockery)
library(tibble)

test_that("localize_to_time_zone works on a trivial data set", {
  mockery::stub(
    where = localize_to_time_zone,
    what = "get_timezome_id",
    how = "America/Los_Angeles"
  )

  df <- act_tbl(tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  ))
  local_df <- localize_to_time_zone(df)
  expect_equal(nrow(local_df), 1)
  expect_equal(tz(local_df$time), "America/Los_Angeles")
  expect_equal(hour(local_df$time), 16)
})

test_that("localize_to_time_zone fails if it's not an act_tbl", {
  df <- tibble::tribble(
    ~time, ~lat, ~lon, ~ele,
    lubridate::ymd_hms("2019-01-01 0:00:01"), 1, 1, 10000
  )
  expect_error(localize_to_time_zone(df))
})

test_that("localize_to_time_zone fails if the act_tbl doesn't contain times", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    1, 1
  ))
  expect_error(localize_to_time_zone(df))
})

test_that("get_timezome_id errors when there's no google key", {
  mockery::stub(
    where = get_timezome_id,
    what = "has_google_key",
    how = FALSE
  )

  expect_error(get_timezome_id(1, 2, 3))
})

test_that("get_timezome_id detects non-JSON responses", {
  mockery::stub(
    where = get_timezome_id,
    what = "has_google_key",
    how = TRUE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "google_key",
    how = "key"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::GET",
    how = httr:::response(
      status_code = 200,
      content = "{timeZoneId:\"America/Los_Angeles\"}"
    )
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_type",
    how = "application/xml"
  )

  expect_error(get_timezome_id(1, 2, 3))
})

test_that("get_timezome_id handles HTTP errors", {
  mockery::stub(
    where = get_timezome_id,
    what = "has_google_key",
    how = TRUE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "google_key",
    how = "key"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::GET",
    how = httr:::response(
      status_code = 200,
      content = "{timeZoneId:\"America/Los_Angeles\"}"
    )
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_type",
    how = "application/json"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_error",
    how = TRUE
  )

  expect_error(get_timezome_id(1, 2, 3))
})


test_that("get_timezome_id handles HTTP errors", {
  mockery::stub(
    where = get_timezome_id,
    what = "has_google_key",
    how = TRUE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "google_key",
    how = "key"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::GET",
    how = httr:::response(
      status_code = 200,
      content = "{timeZoneId:\"America/Los_Angeles\"}"
    )
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_type",
    how = "application/json"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_error",
    how = FALSE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::content",
    how = tibble(
      status = "REQUEST_DENIED",
      errorMessage = "The provided API key is invalid."
    )
  )

  expect_error(get_timezome_id(1, 2, 3))
})

test_that("get_timezome_id works in normal circumstances", {
  mockery::stub(
    where = get_timezome_id,
    what = "has_google_key",
    how = TRUE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "google_key",
    how = "key"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::GET",
    how = httr:::response(
      status_code = 200,
      content = "{timeZoneId:\"America/Los_Angeles\"}"
    )
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_type",
    how = "application/json"
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::http_error",
    how = FALSE
  )

  mockery::stub(
    where = get_timezome_id,
    what = "httr::content",
    how = tibble(status = "OK", timeZoneId = "America/Los_Angeles")
  )

  tzid <- get_timezome_id(1, 2, 3)
  expect_equal(tzid, "America/Los_Angeles")
})
