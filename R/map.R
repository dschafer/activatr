#' Computes the correct google maps zoom for a given Activatr DF.
#'
#' Logic is taken from https://goo.gl/gZsU4W.
#'
#' @param df A Activatr DF: a tibble from \code{parse_gpx} or \code{parse_tcx}.
#' @return An int to use as the \code{zoom} value in
#'        \code{ggmap::get_googlemap}.
#'
#' @importFrom geosphere distm distHaversine
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

#' Get a ggmap object for a given Activatr DF.
#'
#' Note that since this calls \code{ggmap::get_googlemap}, you must have
#' previously called \code{ggmap::register_google} to register an API key.
#'
#' @param df A Activatr DF: a tibble from \code{parse_gpx} or \code{parse_tcx}.
#' @param ... Additional arguments to pass to \code{ggmap::get_googlemap}.
#' @return A ggmap object, the result of calling \code{ggmap::get_googlemap},
#'   but with the correct center and size to include the entire data frame.
#'
#' @importFrom ggmap get_googlemap
#' @export
get_ggmap_from_df <- function(df, ...) {
  df <- act_tbl(df)

  center <- c(
    mean(c(max(df$lon), min(df$lon))),
    mean(c(max(df$lat), min(df$lat)))
  )
  zoom <- get_zoom(df)
  get_googlemap(center = center, zoom = zoom, ...)
}
