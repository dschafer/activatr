#' The result of calling get_ggmap_from_df on running_example
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
#' except using that in vignettes or examples is hard, because
#' \code{get_ggmap_from_df} requires an api key be passed to `ggmap`.
#' So this is the result of running that with a valid API key.
"running_example_ggmap"
