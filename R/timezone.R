#' Localize time zone values
#'
#' `localize_to_time_zone` uses Google Maps Time Zone APIs to localize the
#' time zone in an [`act_tbl`][act_tbl-class]. This modifies a mutated
#' [`act_tbl`][act_tbl-class] with the time column updated to contain the same
#' absolute time, but with the appropriate time zone for where the activity took
#' place.
#'
#' Note that to avoid overuse of the APIs, this does an "approximation", in that
#' it finds the correct time zone for the first point in the data frame, and
#' assumes all points in that data frame use that time zone. Runs between time
#' zones (or runs that cross daylight savings time shifts) will hence be
#' recorded using a consistent, but not always pointwise correct, timezone.
#'
#' Note that you must have previously called `ggmap::register_google()` to
#' register an API key before calling this.
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#' @importFrom rlang abort
#' @importFrom utils head
#'
#' @export
#'
#' @param df An [`act_tbl`][act_tbl-class] object.
#' @return That same [`act_tbl`][act_tbl-class], but with the `time` column
#'         updated to be in the local time zone rather than UTC.
#'
#' @examples
#' \dontrun{
#' example_gpx_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' act_tbl <- parse_gpx(example_gpx_file)
#' act_tbl_with_tz <- localize_to_time_zone(act_tbl)
#' }
localize_to_time_zone <- function(df) {
  if (!is_act_tbl(df)) {
    abort("df parameter to localize_to_time_zone() must be an act_tbl.")
  }

  if (!("time" %in% colnames(df))) {
    abort("Cannot use localize_to_time_zone with act_tbl lacking time.")
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
