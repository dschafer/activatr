#' `act_tbl` class
#'
#' The `act_tbl` S3 class is a subclass of [`data.frame`][base::data.frame()]
#' and [`tibble`][tibble::tbl_df-class].
#'
#' In nearly every respect, it can be treated like a tibble; however, this
#' allows the package to provide an improved `summary.act_tbl()` function
#' to get an overview of the activity.
#'
#' @name act_tbl-class
#' @aliases act_tbl-class
NULL

#' Create an [`act_tbl`][act_tbl-class].
#'
#' `act_tbl` takes a [`data.frame`][base::data.frame()] or
#' [`tibble`][tibble::tbl_df-class]. and returns an [`act_tbl`][act_tbl-class]
#' object.
#'
#' While the most common way to get an [`act_tbl`][act_tbl-class] is by calling
#' `parse_gpx()` or `parse_tcx()`, it's possible to have a
#' [`data.frame`][base::data.frame()] or [`tibble`][tibble::tbl_df-class] from
#' another source, but still want to use the `summary.act_tbl()` functionality.
#'
#' This method validates that the provided [`tibble`][tibble::tbl_df-class]
#' represents an activity (specifically, that it contains a `lat` and `lon`
#' column, each of which is a double). If so, it returns an
#' [`act_tbl`][act_tbl-class] representation of the provided input. If not, it
#' throws an error.
#'
#' @param x A [`tibble`][tibble::tbl_df-class] to turn into an `act_tbl`.
#' @return An S3 object of class `"act_tbl"`, or errors if the provided tibble
#'         is invalid.
#'
#' @examples
#' df <- tibble::tibble(lon = c(37.8, 37.9), lat = c(-122.8, -122.9))
#' the_act_tbl <- activatr:::act_tbl(df)
#' class(the_act_tbl)
#'
#' @noRd
act_tbl <- function(x) {
  validate_act_tbl(new_act_tbl(x))
}

#' Constructor for `act_tbl`.
#'
#' `new_act_tbl` constructs a `act_tbl` object.
#'
#' @param x An object to turn into an `act_tbl`.
#' @return `new_act_tbl` returns an object of class `"act_tbl"`.
#' @importFrom tibble is_tibble
#'
#' @noRd
new_act_tbl <- function(x) {
  stopifnot(is_tibble(x))
  structure(x, class = c("act_tbl", class(x)))
}

#' Validator for `act_tbl`.
#'
#' `validate_act_tbl` takes a newly constructed `act_tbl` object
#' and ensures that it is valid: namely, that it has a lat and lon column.
#'
#' @param act_tbl A candidate that might be an `act_tbl`.
#' @return `validate_act_tbl` returns its input, or errors if the
#'         provided tibble is not suitable to be an `"act_tbl"`, i.e.
#'         it does not have lat and lon columns.
#'
#' @noRd
validate_act_tbl <- function(act_tbl) {
  stopifnot("lat" %in% colnames(act_tbl))
  stopifnot("lon" %in% colnames(act_tbl))
  act_tbl
}

#' Tests if the input is a `act_tbl`.
#'
#' `is_act_tbl` tests if a given object is an `act_tbl`.
#'
#' @param act_tbl A candidate that might be an `act_tbl`.
#' @return `is_act_tbl` returns TRUE iff `act_tbl` is a
#'         `act_tbl`.
#'
#' @noRd
is_act_tbl <- function(act_tbl) {
  inherits(act_tbl, "act_tbl")
}

#' Summarizes [`act_tbl`][act_tbl-class] objects.
#'
#' `summary.act_tbl` returns a tibble with canonical information about
#' the activity.
#'
#' This is designed to allow for easy creation of activity summary data
#' sets by mapping summary over each [`act_tbl`][act_tbl-class] then using
#' `dplyr::bind_rows()`, `purrr::map_dfr()`, or equivalent to create a complete
#' data set.
#'
#' @export
#'
#' @param object an object for which a summary is desired
#' @param full Whether every column should be included, and filled with NA if
#'             missing. Most useful to ensure the tibble has the same shape for
#'             every file, allowing eventual use of `dplyr::bind_rows()` or
#'             `purrr::map_dfr()` to create a full summary data set.
#' @param units
#' Which units should be used?
#' * "imperial" returns distance in miles, pace in minutes per mile, and
#'   elevation in feet.
#' * "metric" returns distance in kilometers, pace in minutes per kilometer, and
#'   elevation in meters.
#' @param ... Additional arguments.
#' @return Returns a tibble with a single row, containing a summary of the given
#'         [`act_tbl`][act_tbl-class].
#' @importFrom dplyr lag mutate pull slice
#' @importFrom lubridate as.duration dseconds
#' @importFrom slider slide_index_mean
#' @importFrom tibble tribble
#'
#' @examples
#' example_gpx_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' act_tbl <- parse_gpx(example_gpx_file)
#' summary(act_tbl)
#'
#' \dontrun{
#' files <- list.files("path/to/many/files", pattern = "*.gpx")
#' gpxs <- files |> purrr::map(\(f) parse_gpx(f))
#' summaries <- gpxs |> purrr::map_dfr(\(g) summary(g, full = TRUE))
#' }
#'
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
  distance <- object |>
    mutate_with_distance() |>
    pull(.data$distance) |>
    sum(na.rm = TRUE) / distance_factor
  summary <- tribble(~Distance, distance)

  if (full) {
    # Fill in the columns in advance
    summary <- summary |> mutate(
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
    date <- object |>
      slice(1) |>
      pull(time)

    time <- object |>
      mutate(time_diff = .data$time - lag(.data$time)) |>
      pull(.data$time_diff) |>
      sum(na.rm = TRUE) |>
      as.duration()
    summary <- summary |> mutate(Date = date, Time = time)

    paces <- object |>
      mutate_with_speed() |>
      mutate(pace = pace_converter(.data$speed)) |>
      pull(.data$pace)
    avgpace <- (time / distance) |> as.duration()
    maxpace <- min(paces, na.rm = TRUE) |> # min since low paces are faster
      as.duration()

    summary <- summary |> mutate(AvgPace = avgpace, MaxPace = maxpace)
  }

  # If we have elevation, add AvgElev, ElevGain and ElevLoss
  if ("ele" %in% colnames(object)) {
    # We use a rolling 30 second average of elevation to smooth out jitter.
    period <- dseconds(min(15, nrow(object)))
    elev_df <- object |>
      mutate(
        ele = slide_index_mean(
          .data$ele,
          .data$time,
          before = period,
          after = period
        )
      ) |>
      mutate(
        elevdelta = .data$ele - lag(.data$ele),
        elevgain = ifelse(.data$elevdelta > 0, .data$elevdelta, 0),
        elevloss = ifelse(.data$elevdelta < 0, -.data$elevdelta, 0)
      )
    elevgain <- elev_df |>
      pull(.data$elevgain) |>
      sum(na.rm = TRUE)
    elevloss <- elev_df |>
      pull(.data$elevloss) |>
      sum(na.rm = TRUE)
    avgelev <- object |>
      pull(.data$ele) |>
      mean(na.rm = TRUE)
    summary <- summary |> mutate(
      ElevGain = ele_converter(elevgain),
      ElevLoss = ele_converter(elevloss),
      AvgElev = ele_converter(avgelev)
    )
  }

  # If we have hr, add AvgHR and MaxHR
  if ("hr" %in% colnames(object)) {
    avghr <- object |>
      pull(.data$hr) |>
      mean(na.rm = TRUE)
    maxhr <- object |>
      pull(.data$hr) |>
      max(na.rm = TRUE)
    summary <- summary |> mutate(AvgHR = avghr, MaxHR = maxhr)
  }

  # If we have cad, add AvgCad and MaxCad
  if ("cad" %in% colnames(object)) {
    avgcad <- object |>
      pull(.data$cad) |>
      mean(na.rm = TRUE)
    maxcad <- object |>
      pull(.data$cad) |>
      max(na.rm = TRUE)
    summary <- summary |> mutate(AvgCadence = avgcad, MaxCadence = maxcad)
  }

  # If we have a title, add it in
  if (!is.null(attr(object, "title"))) {
    summary <- summary |> mutate(Title = attr(object, "title"))
  }

  summary
}
