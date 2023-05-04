#' Precomputed example ggmap
#'
#' This is a precomputed ggmap for running_example.gpx, to avoid needing an
#' API key in vignettes, which `get_ggmap_from_df()` requires.
#'
#' This is the result of running:
#'
#' ```
#' running_file <- system.file(
#'   "extdata",
#'   "running_example.gpx",
#'   package = "activatr")
#' running_df <- parse_gpx(running_file)
#' running_example_ggmap <- get_ggmap_from_df(running_df)
#' ```
#'
#' after setting a valid API key.
#'
#' @keywords internal
"running_example_ggmap"
