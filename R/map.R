#' Get a map for a given [`act_tbl`][act_tbl-class]
#'
#' `get_ggmap_from_df` takes an [`act_tbl`][act_tbl-class] object, computes the
#' correct zoom and center for that activity, then returns a `ggmap` object for
#' that zoom and center.
#'
#' Note that since this calls `ggmap::get_googlemap()`, you must have
#' previously called `ggmap::register_google()` to register an API key.
#'
#' @param df An [`act_tbl`][act_tbl-class] object.
#' @param ... Additional arguments forwarded to `ggmap::get_googlemap()`.
#' @return A ggmap object, the result of calling `ggmap::get_googlemap()`,
#'         with the correct center and size to include the entire activity
#'         represented by the [`act_tbl`][act_tbl-class].
#'
#' @seealso `ggmap::get_googlemap()`
#'
#' @importFrom ggmap get_googlemap
#' @export
#'
#' @examples
#' \dontrun{
#' example_gpx_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' act_tbl <- parse_gpx(example_gpx_file)
#' ggmap::ggmap(get_ggmap_from_df(act_tbl))
#' }
get_ggmap_from_df <- function(df, ...) {
  df <- act_tbl(df)

  center <- c(
    mean(c(max(df$lon), min(df$lon))),
    mean(c(max(df$lat), min(df$lat)))
  )
  zoom <- get_zoom(df)
  get_googlemap(center = center, zoom = zoom, ...)
}

#' Compute Google Maps Zoom
#'
#' `get_zoom` computes the the correct google maps zoom for a given
#' [`act_tbl`][act_tbl-class].
#'
#' Logic is taken from <https://goo.gl/gZsU4W>.
#'
#' @param df An [`act_tbl`][act_tbl-class] object.
#' @return An int to use as the `zoom` parameter to
#'        `ggmap::get_googlemap()`.
#'
#' @importFrom geosphere distm distHaversine
#'
#' @seealso <https://goo.gl/gZsU4W>
#'
#' @noRd
get_zoom <- function(df) {
  df <- act_tbl(df)

  centerlon <- mean(c(max(df$lon), min(df$lon)))
  replat <- max(df$lat)
  lonlengthm <- distm(
    c(centerlon, max(df$lat)), c(centerlon, min(df$lat)),
    fun = distHaversine
  )
  latlengthm <- distm(
    c(min(df$lon), replat), c(max(df$lon), replat),
    fun = distHaversine
  )
  # 620 instead of 640 so there's a 10 pixel buffer
  size <- 620
  radius <- 156543.03392
  zoomlon <- floor(log2(size * radius * cos(replat * pi / 180) / lonlengthm))
  zoomlat <- floor(log2(size * radius * cos(replat * pi / 180) / latlengthm))
  min(zoomlon, zoomlat)
}
