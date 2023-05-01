#' Creates an activatr tibble, abbreviated \code{act_tbl}.
#'
#' \code{act_tbl} takes a tibble and returns an \code{act_tbl} object.
#'
#' @param x An object to turn into an \code{act_tbl}.
#' @return \code{act_tbl} returns an object of class \code{"act_tbl"}, or
#'         errors if the provided tibble is invalid.
#' @rdname act_tbl
act_tbl <- function(x) {
  validate_act_tbl(new_act_tbl(x))
}

#' Constructor for \code{act_tbl}.
#'
#' \code{new_act_tbl} constructs a \code{act_tbl} object.
#'
#' @param x An object to turn into an \code{act_tbl}.
#' @return \code{new_act_tbl} returns an object of class \code{"act_tbl"}.
#' @importFrom tibble is_tibble
#' @rdname act_tbl
#'
#' @noRd
new_act_tbl <- function(x) {
  stopifnot(is_tibble(x))
  structure(x, class = c("act_tbl", class(x)))
}

#' Validator for \code{act_tbl}.
#'
#' \code{validate_act_tbl} takes a newly constructed \code{act_tbl} object
#' and ensures that it is valid: namely, that it has a lat and lon column.
#'
#' @param act_tbl A candidate that might be an \code{act_tbl}.
#' @return \code{validate_act_tbl} returns its input, or errors if the
#'         provided tibble is not suitable to be an \code{"act_tbl"}, i.e.
#'         it does not have lat and lon columns.
#' @rdname act_tbl
#'
#' @noRd
validate_act_tbl <- function(act_tbl) {
  stopifnot("lat" %in% colnames(act_tbl))
  stopifnot("lon" %in% colnames(act_tbl))
  act_tbl
}

#' Tests if the input is a \code{act_tbl}.
#'
#' \code{is_act_tbl} tests if a given object is an \code{act_tbl}.
#'
#' @param act_tbl A candidate that might be an \code{act_tbl}.
#' @return \code{is_act_tbl} returns TRUE iff \code{act_tbl} is a
#'         \code{act_tbl}.
#' @rdname act_tbl
#'
#' @noRd
is_act_tbl <- function(act_tbl) {
  inherits(act_tbl, "act_tbl")
}

#' An S3 method for summary on \code{act_tbl} objects.
#'
#' \code{summary.act_tbl} returns a tibble with canonical information about
#' the activity. Designed to allow for easy creation of activity summary data
#' sets by mapping summary over each \code{act_tbl} then using \code{bind_rows}
#' to create a complete data set.
#'
#' @export
#'
#' @param object an object for which a summary is desired
#' @param full Whether every column should be included, and filled with NA if
#'             missing. Most useful to ensure the tibble has the same shape for
#'             every file, allowing eventual use of \code{bind_rows} to create
#'             a full summary data set.
#' @param units Which units should be used?
#'              Imperial returns distance in miles, pace in minutes per mile,
#'              and elevation in feet.
#'              Metric returns distance in kilometers, pace in minutes per
#'              kilometer, and elevation in meters.
#' @param ... Additional arguments.
#' @return \code{summary.act_tbl} returns a tibble with a single row,
#'         containing a summary of the given \code{act_tbl}.
#' @importFrom dplyr lag mutate pull slice
#' @importFrom lubridate as.duration dseconds
#' @importFrom slider slide_index_dbl
#' @importFrom tibble tribble
#' @rdname act_tbl
summary.act_tbl <- function(object,
                            full = FALSE,
                            units = c("imperial", "metric"),
                            ...) {
  units <- match.arg(units)

  distance_factor <- if (units == "imperial") {
    mile_in_meters
  } else {
    kilometer_in_meters
  }
  pace_converter <- if (units == "imperial") {
    speed_to_mile_pace
  } else {
    speed_to_kilometer_pace
  }
  ele_converter <- if (units == "imperial") {
    meters_to_feet
  } else {
    identity
  }

  # Populate it with distance
  distance <- object %>%
    mutate_with_distance() %>%
    pull(.data$distance) %>%
    sum(na.rm = TRUE) / distance_factor
  summary <- tribble(~Distance, distance)

  if (full) {
    # Fill in the columns in advance
    summary <- summary %>% mutate(
      Date = NA,
      Time = NA,
      AvgPace = NA,
      MaxPace = NA,
      ElevGain = NA,
      ElevLoss = NA,
      AvgElev = NA,
      AvgHR = NA,
      MaxHR = NA,
      AvgCadence = NA,
      MaxCadence = NA,
      Title = NA
    )
  }

  # If we have time, add date, time, and pace
  if ("time" %in% colnames(object)) {
    date <- object %>%
      slice(1) %>%
      pull(time)

    time <- object %>%
      mutate(time_diff = .data$time - lag(.data$time)) %>%
      pull(.data$time_diff) %>%
      sum(na.rm = TRUE) %>%
      as.duration()
    summary <- summary %>% mutate(Date = date, Time = time)

    paces <- object %>%
      mutate_with_speed() %>%
      mutate(pace = pace_converter(.data$speed)) %>%
      pull(.data$pace)
    avgpace <- (time / distance) %>% as.duration()
    maxpace <- min(paces, na.rm = TRUE) %>% # min since low paces are faster
      as.duration()

    summary <- summary %>% mutate(AvgPace = avgpace, MaxPace = maxpace)
  }

  # If we have elevation, add AvgElev, ElevGain and ElevLoss
  if ("ele" %in% colnames(object)) {
    # We use a rolling 30 second average of elevation to smooth out jitter.
    period <- dseconds(min(15, nrow(object)))
    elev_df <- object %>%
      mutate(
        ele = slide_index_dbl(
          .data$ele,
          .data$time,
          mean,
          .before = period,
          .after = period
        )
      ) %>%
      mutate(
        elevdelta = .data$ele - lag(.data$ele),
        elevgain = ifelse(.data$elevdelta > 0, .data$elevdelta, 0),
        elevloss = ifelse(.data$elevdelta < 0, -.data$elevdelta, 0)
      )
    elevgain <- elev_df %>%
      pull(.data$elevgain) %>%
      sum(na.rm = TRUE)
    elevloss <- elev_df %>%
      pull(.data$elevloss) %>%
      sum(na.rm = TRUE)
    avgelev <- object %>%
      pull(.data$ele) %>%
      mean(na.rm = TRUE)
    summary <- summary %>% mutate(
      ElevGain = ele_converter(elevgain),
      ElevLoss = ele_converter(elevloss),
      AvgElev = ele_converter(avgelev)
    )
  }

  # If we have hr, add AvgHR and MaxHR
  if ("hr" %in% colnames(object)) {
    avghr <- object %>%
      pull(.data$hr) %>%
      mean(na.rm = TRUE)
    maxhr <- object %>%
      pull(.data$hr) %>%
      max(na.rm = TRUE)
    summary <- summary %>% mutate(AvgHR = avghr, MaxHR = maxhr)
  }

  # If we have cad, add AvgCad and MaxCad
  if ("cad" %in% colnames(object)) {
    avgcad <- object %>%
      pull(.data$cad) %>%
      mean(na.rm = TRUE)
    maxcad <- object %>%
      pull(.data$cad) %>%
      max(na.rm = TRUE)
    summary <- summary %>% mutate(AvgCadence = avgcad, MaxCadence = maxcad)
  }

  # If we have a title, add it in
  if (!is.null(attr(object, "title"))) {
    summary <- summary %>% mutate(Title = attr(object, "title"))
  }

  summary
}
