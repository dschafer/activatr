context("Mapping Activatr DFs")
library(activatr)

# Necessary because of https://github.com/dkahle/ggmap/issues/270
library(ggmap)

test_that("zoom is 9 for one degree at the equator", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    0, 0,
    1, 1
  ))
  expect_equal(activatr:::get_zoom(df), 9)
})

test_that("zoom is 8 for two degrees at the equator", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    0, 0,
    2, 2
  ))
  expect_equal(activatr:::get_zoom(df), 8)
})

test_that("Map URL is correct", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    0, 0,
    2, 2
  ))
  url <- get_ggmap_from_df(df, urlonly = TRUE)
  expect_equal(url, "https://maps.googleapis.com/maps/api/staticmap?center=1,1&zoom=8&size=640x640&scale=2&maptype=terrain") # nolint URL is long
})

test_that("Custom parameters get forwarded", {
  df <- act_tbl(tibble::tribble(
    ~lat, ~lon,
    0, 0,
    2, 2
  ))
  url <- get_ggmap_from_df(df, urlonly = TRUE, maptype = "hybrid")
  expect_equal(url, "https://maps.googleapis.com/maps/api/staticmap?center=1,1&zoom=8&size=640x640&scale=2&maptype=hybrid") # nolint URL is long
})
