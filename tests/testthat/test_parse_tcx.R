context("Parsing TCX")
library(activatr)

running_tcx_file <- system.file(
  "extdata",
  "running_example.tcx.gz",
  package = "activatr"
)

test_that("default parsing works", {
  df <- parse_tcx(running_tcx_file)
  expect_equal(nrow(df), 4442)
})

test_that("every with 1 is the same as normal", {
  df <- parse_tcx(running_tcx_file)
  df_every_1 <- parse_tcx(running_tcx_file, every = 1)
  expect_equal(nrow(df_every_1), nrow(df))
})

test_that("parsing with every skips entries", {
  df <- parse_tcx(running_tcx_file, every = 10)
  expect_equal(nrow(df), 440)
})

test_that("every rejects zero", {
  expect_error(parse_tcx(running_tcx_file, every = 0))
})

test_that("every rejects negatives", {
  expect_error(parse_tcx(running_tcx_file, every = -1))
})

test_that("every rejects decimals", {
  expect_error(parse_tcx(running_tcx_file, every = 1.5))
})

test_that("default parsing is basic", {
  df <- parse_tcx(running_tcx_file, every = 100)
  basic_df <- parse_tcx(running_tcx_file, detail = "basic")
  expect_equal(colnames(df), colnames(basic_df))
})

test_that("basic parsing has expected columns", {
  df <- parse_tcx(running_tcx_file, detail = "basic", every = 100)
  expect_equal(colnames(df), c("lat", "lon", "ele", "time"))
})

test_that("latlon parsing has expected columns", {
  df <- parse_tcx(running_tcx_file, detail = "latlon", every = 100)
  expect_equal(colnames(df), c("lat", "lon"))
})

test_that("advanced parsing has expected columns", {
  df <- parse_tcx(running_tcx_file, detail = "advanced", every = 100)
  expect_equal(
    colnames(df),
    c("lat", "lon", "ele", "time", "hr", "cad")
  )
})

test_that("invalid parsing errors", {
  expect_error(
    parse_tcx(running_tcx_file, detail = "nonvalidstring", every = 100)
  )
})
