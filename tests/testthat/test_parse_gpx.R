context("Parsing GPX")
library(activatr)

running_gpx_file <- system.file(
  "extdata",
  "running_example.gpx.gz",
  package = "activatr"
)

test_that("default parsing works", {
  df <- parse_gpx(running_gpx_file)
  expect_equal(nrow(df), 4433)
})

test_that("every with 1 is the same as normal", {
  df <- parse_gpx(running_gpx_file)
  df_every_1 <- parse_gpx(running_gpx_file, every = 1)
  expect_equal(nrow(df_every_1), nrow(df))
})

test_that("parsing with every skips entries", {
  df <- parse_gpx(running_gpx_file, every = 10)
  expect_equal(nrow(df), 443)
})

test_that("every rejects zero", {
  expect_error(parse_gpx(running_gpx_file, every = 0))
})

test_that("every rejects negatives", {
  expect_error(parse_gpx(running_gpx_file, every = -1))
})

test_that("every rejects decimals", {
  expect_error(parse_gpx(running_gpx_file, every = 1.5))
})

test_that("default parsing is basic", {
  df <- parse_gpx(running_gpx_file, every = 100)
  basic_df <- parse_gpx(running_gpx_file, detail = "basic")
  expect_equal(colnames(df), colnames(basic_df))
})

test_that("basic parsing has expected columns", {
  df <- parse_gpx(running_gpx_file, detail = "basic", every = 100)
  expect_equal(colnames(df), c("lat", "lon", "ele", "time"))
})

test_that("latlon parsing has expected columns", {
  df <- parse_gpx(running_gpx_file, detail = "latlon", every = 100)
  expect_equal(colnames(df), c("lat", "lon"))
})

test_that("advanced parsing has expected columns", {
  df <- parse_gpx(running_gpx_file, detail = "advanced", every = 100)
  expect_equal(
    colnames(df),
    c("lat", "lon", "ele", "time", "hr", "cad")
  )
})

test_that("invalid parsing errors", {
  expect_error(
    parse_gpx(running_gpx_file, detail = "nonvalidstring", every = 100)
  )
})

test_that("attributes are included", {
  df <- parse_gpx(running_gpx_file, detail = "basic", every = 100)
  expect_equal(attr(df, "title"), "Sunrise 15K PR (sub-8:00)")
})

test_that("extract_metainfo correctly extracts subelements", {
  xmldoc <- read_xml("<a><b><c>1</c></b><b><c>2</c></b></a>")
  trackpoints <- xml_find_all(xmldoc, "b")
  expect_equal(extract_metainfo("filename.xml", trackpoints, "c"), c("1", "2"))
})

test_that("extract_metainfo correctly returns NA on missing elements", {
  xmldoc <- read_xml("<a><b><c>1</c></b><b /></a>")
  trackpoints <- xml_find_all(xmldoc, "b")
  expect_equal(extract_metainfo("filename.xml", trackpoints, "c"), c("1", NA))
})
