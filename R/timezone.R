
#' Uses Google Maps Time Zone APIs to localize the time zone.
#'
#' This returns a mutated Activatr DF with the time column updated to reflect
#' the correct time zone, using the Google Maps Time Zone APIs.
#'
#' Note that to avoid overuse of the API, this does an "approximation", in that
#' it finds the correct time zone for the first point in the data frame, and
#' assumes all points in that data frame use that time zone. Runs between time
#' zones (or runs that cross daylight savings time shifts) will hence be
#' recorded using a consistent, but not always pointwise correct, timezone.
#'
#' Note that you must have previously called \code{ggmap::register_google} to
#' register an API key before calling this.
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#' @importFrom rlang abort
#' @importFrom utils head
#'
#' @export
#'
#' @param df A Activatr DF: a tibble from \code{parse_gpx} or \code{parse_tcx}.
#' @return That same Activatr DF, but with the \code{time} column updated to be
#'         in the local time zone rather than UTC.
localize_to_time_zone <- function(df) {
  if (!is_act_tbl(df)) {
    abort("df parameter to localize_to_time_zone() must be an Activatr DF.")
  }

  if (!("time" %in% colnames(df))) {
    abort("Cannot use localize_to_time_zone with Activatr DF lacking time.")
  }

  lat <- head(df, 1)$lat
  lon <- head(df, 1)$lon
  ts <- as.integer(head(df, 1)$time)

  time_zone_id <- get_timezome_id(lat, lon, ts)

  mutate(df, time = with_tz(.data$time, tzone = time_zone_id))
}

#' Uses Google Maps Time Zone APIs to get a timezone ID
#'
#' Given a lat, lon, and timestamp, returns the appropriate timezone ID.
#'
#' @importFrom ggmap has_google_key google_key scrub_key
#' @importFrom glue glue
#' @importFrom httr GET http_type http_error status_code
#' @importFrom rlang abort
#'
#' @param lat The latitude of the point
#' @param lon The logitude of the point
#' @param ts A unix timestamp representing the point in time, in UTC
#' @return a string containing the ID of the time zone, such as
#'         "America/Los_Angeles" or "Australia/Sydney".
#'
#' @noRd
get_timezome_id <- function(lat, lon, ts) {
  if (has_google_key()) {
    key <- google_key() # nolint (unused variable)
  } else {
    abort(c(
      "Cannot use localize_to_time_zone before calling ",
      "ggmap::register_google()"
    ))
  }

  url <- glue("https://maps.googleapis.com/maps/api/timezone/json?location={lat},{lon}&timestamp={ts}&key={key}") # nolint Long URL
  message("Source : ", scrub_key(url))
  response <- httr::GET(url)
  if (httr::http_type(response) != "application/json") {
    abort("API did not return json")
  }
  content <- httr::content(response)
  if (httr::http_error(response) || content$status != "OK") {
    error_message <- sprintf(
      "Google Maps API request failed [%s]\n%s\n<%s>",
      httr::status_code(response),
      content$status,
      content$errorMessage
    )
    abort(error_message)
  }
  time_zone_id <- content$timeZoneId
  time_zone_id
}
