#' Augments a Activatr DF with a distance variable.
#'
#' This returns a mutated Activatr DF with a new column representing distance,
#' in meters. The distance is determined by looking at the lat/lon delta
#' between the current point and the previous point: hence, it is always NA
#' for the first row in the data frame.
#'
#' @importFrom dplyr lag lead mutate rowwise ungroup
#' @importFrom geosphere distm
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom rlang abort .data
#'
#' @param df A Activatr DF: a tibble from \code{parse_gpx} or \code{parse_tcx}.
#' @param method If 2D (default), ignores elevation. If 3D, includes elevation.
#' @param lead How far ahead to look for the "end" point
#' @param lag How far behind to look for the "start" point
#' @return That same Activatr DF, but with a new \code{distance} column, in
#'        meters.
mutate_with_distance <- function(
                                 df,
                                 method = c("2D", "3D"),
                                 lead = 0,
                                 lag = 1) {
  method <- match.arg(method)

  df <- act_tbl(df)

  if (method == "3D" && !("ele" %in% colnames(df))) {
    abort("Cannot use 3D speed method with Activatr DF lacking elevation.")
  }

  df2d <- df %>%
    mutate(
      start_lat = dplyr::lag(.data$lat, lag),
      start_lon = dplyr::lag(.data$lon, lag),
      end_lat = dplyr::lead(.data$lat, lead),
      end_lon = dplyr::lead(.data$lon, lead),
      isna = (is.na(.data$start_lat) |
        is.na(.data$start_lon) |
        is.na(.data$end_lat) |
        is.na(.data$end_lon))
    ) %>%
    rowwise() %>%
    mutate(
      dist2d = ifelse(
        .data$isna,
        NA,
        distm(
          c(.data$end_lon, .data$end_lat),
          c(.data$start_lon, .data$start_lat)
        )
      )
    ) %>%
    ungroup()

  if (method == "2D") {
    return(df %>% mutate(distance = df2d$dist2d))
  }

  df3d <- df2d %>%
    mutate(
      start_ele = dplyr::lag(.data$ele, lag),
      end_ele = dplyr::lead(.data$ele, lead),
      isna = (.data$isna | is.na(.data$start_ele) | is.na(.data$end_ele))
    ) %>%
    rowwise() %>%
    mutate(
      dist3d = ifelse(
        .data$isna,
        NA,
        sqrt(.data$dist2d**2 + (.data$end_ele - .data$start_ele)**2)
      )
    ) %>%
    ungroup()

  df %>% mutate(distance = df3d$dist3d)
}

#' Augments a Activatr DF with a speed variable.
#'
#' This returns a mutated Activatr DF with a new column representing speed, in
#' meters per second. The speed is determined by looking at the time difference
#' between the current point and the previous point: hence, it is always NA
#' for the first row in the data frame.
#'
#' @importFrom dplyr lag lead mutate rowwise ungroup
#' @importFrom geosphere distm
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom rlang abort .data
#'
#' @export
#'
#' @param df A Activatr DF: a tibble from \code{parse_gpx} or \code{parse_tcx}.
#' @param method If 2D (default), ignores elevation. If 3D, includes elevation.
#' @param lead How far ahead to look for the "end" point
#' @param lag How far behind to look for the "start" point
#' @return That same Activatr DF, but with a new \code{speed} column, in meters
#'         per second.
mutate_with_speed <- function(df, method = c("2D", "3D"), lead = 0, lag = 1) {
  method <- match.arg(method)

  df <- act_tbl(df)

  if (!("time" %in% colnames(df))) {
    abort("Cannot use mutate_with_speed with Activatr DF lacking time.")
  }

  if (method == "3D" && !("ele" %in% colnames(df))) {
    abort("Cannot use 3D speed method with Activatr DF lacking elevation.")
  }

  df_distance <- df %>%
    mutate_with_distance(method, lead, lag)
  df_speed <- df_distance %>%
    mutate(
      start_time = dplyr::lag(.data$time, lag),
      end_time = dplyr::lead(.data$time, lead),
      time_diff = as.numeric(.data$end_time - .data$start_time, units = "secs")
    ) %>%
    rowwise() %>%
    mutate(speed = .data$distance / .data$time_diff) %>%
    ungroup()

  df %>% mutate(speed = df_speed$speed)
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

#' Converts a speed (in meters per second) to a mile pace
#'
#' @importFrom dplyr na_if
#' @importFrom lubridate dseconds
#' @export
#'
#' @param speed a vector of speed values in meters per second,
#'   as from \code{mutate_with_speed}.
#' @return a corresponding vector of lubridate durations, representing the
#'   mile pace.
#'
#' @examples
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
#'   as from \code{mutate_with_speed}.
#' @return a corresponding vector of lubridate durations, representing the
#'   kilometer pace.
#'
#' @noRd
#'
#' @examples
#' speed_to_mile_pace(1)
speed_to_kilometer_pace <- function(speed) {
  dseconds(kilometer_in_meters / na_if(speed, 0))
}

#' Converts a distance in meters to a distance in miles.
#'
#' @param meters a vector of distance values in meters,
#'   as from \code{mutate_with_distance}.
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

#' A formatter that takes a pace duration and returns a formatted M:SS string.
#'
#' @importFrom lubridate as_datetime hour minute second
#' @export
#'
#' @param pace a lubridate duration.
#' @return a formatted string representing the pace.
#'
#' @examples
#' pace_formatter(lubridate::dseconds(390))
pace_formatter <- function(pace) {
  m <- as.integer(
    60 * hour(as_datetime(pace)) +
      minute(as_datetime(pace))
  )
  s <- as.integer(second(as_datetime(pace)))
  sprintf("%d:%02d", m, s)
}
