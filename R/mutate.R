#' Augments a [`act_tbl`][act_tbl-class] with a distance column
#'
#' This returns a mutated [`act_tbl`][act_tbl-class] with a new column
#' representing distance in meters.
#'
#' The distance is determined by looking at the lat/lon delta
#' between the current point and the previous point: hence, it is always NA
#' for the first row in the data frame.
#'
#' The `lead` and `lag` values are helpful to get "smoother" values, especially
#' if the provided activity file has GPS errors in it.
#'
#' @importFrom dplyr lag lead mutate rowwise ungroup
#' @importFrom geosphere distm
#' @importFrom tibble add_column
#' @importFrom rlang abort .data
#'
#' @param df An [`act_tbl`][act_tbl-class] object.
#' @param method If "2D" (default), ignores elevation. If "3D", includes
#'               elevation. "3D" is not often necessary, but for skiing
#'               activities is likely to yield a more accurate value.
#' @param lead How far ahead to look for the "end" point.
#' @param lag How far behind to look for the "start" point.
#' @return That same [`act_tbl`][act_tbl-class], but with a new `distance`
#'         column, in meters.
#'
#' @noRd
mutate_with_distance <- function(df,
                                 method = c("2D", "3D"),
                                 lead = 0,
                                 lag = 1) {
  method <- match.arg(method)

  df <- act_tbl(df)

  if (method == "3D" && !("ele" %in% colnames(df))) {
    abort("Cannot use 3D speed method with act_tbl lacking elevation.")
  }

  df2d <- df |>
    mutate(
      start_lat = dplyr::lag(.data$lat, lag),
      start_lon = dplyr::lag(.data$lon, lag),
      end_lat = dplyr::lead(.data$lat, lead),
      end_lon = dplyr::lead(.data$lon, lead),
      isna = (is.na(.data$start_lat) |
        is.na(.data$start_lon) |
        is.na(.data$end_lat) |
        is.na(.data$end_lon))
    ) |>
    rowwise() |>
    mutate(
      dist2d = ifelse(
        .data$isna,
        NA,
        distm(
          c(.data$end_lon, .data$end_lat),
          c(.data$start_lon, .data$start_lat)
        )
      )
    ) |>
    ungroup()

  if (method == "2D") {
    return(df |> mutate(distance = df2d$dist2d))
  }

  df3d <- df2d |>
    mutate(
      start_ele = dplyr::lag(.data$ele, lag),
      end_ele = dplyr::lead(.data$ele, lead),
      isna = (.data$isna | is.na(.data$start_ele) | is.na(.data$end_ele))
    ) |>
    rowwise() |>
    mutate(
      dist3d = ifelse(
        .data$isna,
        NA,
        sqrt(.data$dist2d**2 + (.data$end_ele - .data$start_ele)**2)
      )
    ) |>
    ungroup()

  df |> mutate(distance = df3d$dist3d)
}

#' Augments an [`act_tbl`][act_tbl-class] with a speed column
#'
#' This returns a mutated [`act_tbl`][act_tbl-class] with a new column
#' representing speed, in meters per second. See `vignette("pace")` for
#' examples.
#'
#' The speed is determined by looking at the time difference
#' between the current point and the previous point: hence, it is always NA
#' for the first row in the data frame.
#'
#' The `lead` and `lag` values are helpful to get "smoother" values, especially
#' if the provided activity file has GPS errors in it.
#'
#' @importFrom dplyr lag lead mutate rowwise ungroup
#' @importFrom geosphere distm
#' @importFrom tibble add_column
#' @importFrom rlang abort .data
#'
#' @export
#'
#' @param df An [`act_tbl`][act_tbl-class] object
#' @param method If "2D" (default), ignores elevation. If "3D", includes
#'               elevation. "3D" is not often necessary, but for skiing
#'               activities is likely to yield a more accurate value.
#' @param lead How far ahead to look for the "end" point.
#' @param lag How far behind to look for the "start" point.
#' @return That same [`act_tbl`][act_tbl-class], but with a new `speed` column,
#'         in meters per second.
#'
#' @examples
#' example_gpx_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' example_act_tbl <- parse_gpx(example_gpx_file)
#' example_act_tbl_with_speed <- mutate_with_speed(example_act_tbl)
#' example_act_tbl_with_speed
mutate_with_speed <- function(df, method = c("2D", "3D"), lead = 0, lag = 1) {
  method <- match.arg(method)

  df <- act_tbl(df)

  if (!("time" %in% colnames(df))) {
    abort("Cannot use mutate_with_speed with act_tbl lacking time.")
  }

  if (method == "3D" && !("ele" %in% colnames(df))) {
    abort("Cannot use 3D speed method with atc_tbl lacking elevation.")
  }

  df_distance <- df |>
    mutate_with_distance(method, lead, lag)
  df_speed <- df_distance |>
    mutate(
      start_time = dplyr::lag(.data$time, lag),
      end_time = dplyr::lead(.data$time, lead),
      time_diff = as.numeric(.data$end_time - .data$start_time, units = "secs")
    ) |>
    rowwise() |>
    mutate(speed = .data$distance / .data$time_diff) |>
    ungroup()

  df |> mutate(speed = df_speed$speed)
}

#' Constant for conversion
#' @noRd
mile_in_meters <- 1609.34
#' Constant for conversion
#' @noRd
kilometer_in_meters <- 1000
#' Constant for conversion
#' @noRd
meters_in_feed <- 3.28084

#' Convert speed to mile pace
#'
#' `speed_to_mile_pace` converts a speed (in meters per second) to a mile pace.
#' This method is vectorized, so it works on a column in a data frame. This is
#' most useful after calling `mutate_with_speed()`, to convert that speed
#' to the more-commonly-used pace. See `vignette("pace")` for examples.
#'
#' @importFrom dplyr na_if
#' @importFrom lubridate dseconds
#' @export
#'
#' @param speed A vector of doubles representing speed in meters per second,
#'   as from `mutate_with_speed()`.
#' @return A corresponding vector of `lubridate::duration` values,
#' representing the mile pace.
#'
#' @examples
#' speed_to_mile_pace(3)
#' speed_to_mile_pace(1)
speed_to_mile_pace <- function(speed) {
  dseconds(mile_in_meters / na_if(speed, 0))
}

#' Converts a speed (in meters per second) to a kilometer pace
#'
#' @importFrom dplyr na_if
#' @importFrom lubridate dseconds
#'
#' @param speed a vector of speed values in meters per second,
#'   as from `mutate_with_speed`.
#' @return a corresponding vector of lubridate durations, representing the
#'   kilometer pace.
#'
#' @noRd
#'
#' @examples
#' speed_to_kilometer_pace(3)
#' speed_to_kilometer_pace(1)
speed_to_kilometer_pace <- function(speed) {
  dseconds(kilometer_in_meters / na_if(speed, 0))
}

#' Converts a distance in meters to a distance in miles.
#'
#' @param meters a vector of distance values in meters,
#'   as from `mutate_with_distance`.
#' @return a corresponding vector of distance values in miles.
#'
#' @noRd
#'
#' @examples
#' meters_to_miles(5000)
meters_to_miles <- function(meters) {
  meters / mile_in_meters
}

#' Converts an elevation in meters to an elevation in feed.
#'
#' @param meters a vector of elevation values in meters,
#' @return a corresponding vector of elevation values in feed
#'
#' @noRd
#'
#' @examples
#' meters_to_feet(5000)
meters_to_feet <- function(meters) {
  meters * meters_in_feed
}

#' Format pace durations
#'
#' `pace_formatter` takes a pace duration and returns a formatted string.
#'
#' This is most useful when plotting pace as one of the axes in a graph; rather
#' than having the "number of seconds" as the axis value, this method can
#' convert that to a more readable format.
#'
#' Most commonly, using something like
#' `ggplot2::scale_y_reverse(label = pace_formatter)` will ensure the y-axis
#' goes from "slowest" to "fastest", and shows paces like "8:30" rather than
#' "510"
#'
#' @importFrom lubridate as_datetime hour minute second
#' @export
#'
#' @param pace A lubridate duration, returned by `lubridate::duration` or
#'             other methods in that family.
#' @return A formatted string representing the pace.
#'
#' @examples
#' pace_formatter(lubridate::dseconds(380))
#' pace_formatter(lubridate::dseconds(510))
#' pace_formatter(lubridate::dseconds(680))
pace_formatter <- function(pace) {
  m <- as.integer(
    60 * hour(as_datetime(pace)) +
      minute(as_datetime(pace))
  )
  s <- as.integer(second(as_datetime(pace)))
  sprintf("%d:%02d", m, s)
}
