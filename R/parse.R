#' Parses a GPX file into a [`act_tbl`][act_tbl-class]
#'
#' This parses a standard GPS Exchange Format XML (GPX) file into an
#' data frame with class [`act_tbl`][act_tbl-class]. See `vignette("parsing")`
#' for examples.
#'
#' @param filename The GPX file to parse
#' @param detail How much detail to parse from the GPX.
#'
#'   * If `basic` (the default), this will parse `lat` / `lon` /
#'   `ele` / `time` columns.
#'   * If `latlon`, this will only parse `lat`/`lon`. This is particularly
#'   useful for GPX files exported without time information, such as from
#'   Strava.
#'   * If `advanced`, it will load everything from basic, plus `hr` /
#'   `cad`. This is most useful for files that have heart rate and cadence
#'   information.
#' @param every Optional. If provided, determines how frequently points will
#'   be sampled from the file, so if 10 is provided, every tenth point will
#'   be selected. If omitted or set to 1, every point will be selected. Must
#'   be a positive integer.
#'
#'   This is most useful to quickly analyze a large file, since parsing is much
#'   faster when skipping 90% of the data points.
#' @return A [`act_tbl`][act_tbl-class] with one row for each trackpoint in the
#' .  GPX (modified by `every`), and with the columns determined by `detail`.
#'
#'  \item{lat}{Latitude, a double in degrees between -90 and 90.}
#'  \item{lon}{Longitude, a double in degrees between -180 and 180.}
#'  \item{ele}{Elevation, a double in meters.}
#'  \item{time}{A date-time representing the time of the point.}
#'  \item{hr}{Heart rate, an int in beats per minute.}
#'  \item{cad}{Cadence, an int in one-foot steps per minute.}
#'
#'  Additionally, attributes are set on the returned object containing top level
#'  data from the GPX. Each of these will be NA when not provided in the file.
#'
#'  \item{filename}{The filename this was parsed from, a string. This is always
#'                  present, and is always the value of the `filename`
#'                  parameter.}
#'  \item{time}{A date-time representing the time of the activity.}
#'  \item{title}{A string.}
#'  \item{desc}{A string.}
#'  \item{type}{A string.}
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_find_first
#' @importFrom lubridate ymd_hms
#' @importFrom rlang abort
#' @importFrom tibble tibble
#'
#' @export
#'
#' @seealso <https://en.wikipedia.org/wiki/GPS_Exchange_Format>
#' @seealso <https://www.topografix.com/gpx.asp>
#'
#' @examples
#' example_gpx_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' act_tbl <- parse_gpx(example_gpx_file)
#' print(act_tbl, n = 5)
#' attr(act_tbl, "title")
#'
#' nrow(parse_gpx(example_gpx_file))
#' nrow(parse_gpx(example_gpx_file, every = 100))
#'
#' colnames(parse_gpx(example_gpx_file))
#' colnames(parse_gpx(example_gpx_file, detail = "latlon"))
#' colnames(parse_gpx(example_gpx_file, detail = "advanced"))
parse_gpx <- function(filename,
                      detail = c("basic", "latlon", "advanced"),
                      every = NA) {
  xmldoc <- read_xml(filename)

  if (is.na(every) || every == 1) {
    trackpoints <- xml_find_all(xmldoc, ".//d1:trkpt")
  } else if (!is.numeric(every) ||
    every < 1 || # nolint (indentation_linter)
    !all.equal(every, abs(as.integer(every)))) {
    abort(paste0(
      "parse_gpx was called with every ",
      "as a non-positive integer: ",
      every
    ))
  } else {
    pattern <- paste0(".//d1:trkpt[position() mod ", every, " = 0]")
    trackpoints <- xml_find_all(xmldoc, pattern)
  }

  detail <- match.arg(detail)

  lat <- as.double(xml_attr(trackpoints, "lat"))
  lon <- as.double(xml_attr(trackpoints, "lon"))
  result <- tibble(lat, lon)

  if (detail == "basic" || detail == "advanced") {
    result$ele <- as.double(extract_metainfo(filename, trackpoints, "d1:ele"))
    result$time <- ymd_hms(extract_metainfo(filename, trackpoints, "d1:time"))
  }

  if (detail == "advanced") {
    result$hr <- as.integer(extract_metainfo(filename, trackpoints, "ns3:hr"))
    result$cad <-
      as.integer(extract_metainfo(filename, trackpoints, "ns3:cad"))
  }

  attr(result, "filename") <- filename

  attr(result, "time") <-
    ymd_hms(xml_text(xml_find_first(xmldoc, ".//d1:metadata/d1:time")))
  attr(result, "title") <-
    trimws(xml_text(xml_find_first(xmldoc, ".//d1:trk/d1:name")))
  attr(result, "desc") <-
    trimws(xml_text(xml_find_first(xmldoc, ".//d1:trk/d1:desc")))
  attr(result, "type") <-
    trimws(xml_text(xml_find_first(xmldoc, ".//d1:trk/d1:type")))

  new_act_tbl(result)
}

#' Parses a TCX file into a [`act_tbl`][act_tbl-class]
#'
#' This parses a standard Training Center XML (TCX) file into a
#' data frame with class [`act_tbl`][act_tbl-class]. See `vignette("parsing")`
#' for examples.
#'
#' @param filename The TCX file to parse
#' @param detail How much detail to parse from the TCX
#'
#'   * If `basic` (the default), this will parse `lat` / `lon` /
#'   `ele` / `time` columns.
#'   * If `latlon`, this will only parse `lat`/`lon`. This is particularly
#'   useful for TCX files exported without time information, such as from
#'   Strava.
#'   * If `advanced`, it will load everything from basic, plus `hr` /
#'   `cad`. This is most useful for files that have heart rate and cadence
#'   information.
#' @param every Optional. If provided, determines how frequently points will
#'   be sampled from the file, so if 10 is provided, every tenth point will
#'   be selected. If omitted or set to 1, every point will be selected. Must
#'   be a positive integer.
#'
#'   This is most useful to quickly analyze a large file, since parsing is much
#'   faster when skipping 90% of the data points.
#' @return A [`act_tbl`][act_tbl-class] with one row for each trackpoint in the
#'         TCX (modified by `every`), and with the columns determined by
#'         `detail`.
#'
#'  \item{lat}{Latitude, a double in degrees between -90 and 90.}
#'  \item{lon}{Longitude, a double in degrees between -180 and 180.}
#'  \item{ele}{Elevation, a double in meters.}
#'  \item{time}{A date-time representing the time of the point.}
#'  \item{hr}{Heart rate, an int in beats per minute.}
#'  \item{cad}{Cadence, an int in one-foot steps per minute.}
#'
#'  Additionally, attributes are set on the tibble containing top level data
#'  from the TCX. Each of these will be NA when not provided in the file.
#'
#'  \item{filename}{The filename this was parsed from, a string. This is always
#'                  present, and is always the value of the `filename`
#'                  parameter.}
#'  \item{time}{A date-time representing the time of the activity.}
#'  \item{type}{A string.}
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_find_first
#' @importFrom lubridate ymd_hms
#' @importFrom rlang abort
#' @importFrom tibble tibble
#'
#' @export
#'
#' @seealso <https://en.wikipedia.org/wiki/Training_Center_XML>
#'
#' @examples
#' example_tcx_file <- system.file(
#'   "extdata",
#'   "running_example.tcx.gz",
#'   package = "activatr"
#' )
#' act_tbl <- parse_tcx(example_tcx_file)
#' print(act_tbl, n = 5)
#' attr(act_tbl, "title")
#'
#' nrow(parse_tcx(example_tcx_file))
#' nrow(parse_tcx(example_tcx_file, every = 100))
#'
#' colnames(parse_tcx(example_tcx_file))
#' colnames(parse_tcx(example_tcx_file, detail = "latlon"))
#' colnames(parse_tcx(example_tcx_file, detail = "advanced"))
parse_tcx <- function(filename,
                      detail = c("basic", "latlon", "advanced"),
                      every = NA) {
  xmldoc <- read_xml(filename)

  if (is.na(every) || every == 1) {
    trackpoints <- xml_find_all(xmldoc, ".//d1:Trackpoint[d1:Position]")
  } else if (!is.numeric(every) ||
    every < 1 || # nolint (indentation_linter)
    !all.equal(every, abs(as.integer(every)))) {
    abort(paste0(
      "parse_tcx was called with every ",
      "as a non-positive integer: ",
      every
    ))
  } else {
    pattern <- paste0(".//d1:Trackpoint[position() mod ", every, " = 0]")
    trackpoints <- xml_find_all(xmldoc, pattern)
  }

  detail <- match.arg(detail)


  lat <- as.double(
    extract_metainfo(filename, trackpoints, "d1:LatitudeDegrees")
  )
  lon <- as.double(
    extract_metainfo(filename, trackpoints, "d1:LongitudeDegrees")
  )
  result <- tibble(lat, lon)


  if (detail == "basic" || detail == "advanced") {
    result$ele <- as.double(
      extract_metainfo(filename, trackpoints, "d1:AltitudeMeters")
    )
    result$time <- ymd_hms(
      extract_metainfo(filename, trackpoints, "d1:Time")
    )
  }

  if (detail == "advanced") {
    result$hr <- as.integer(
      extract_metainfo(filename, trackpoints, "d1:HeartRateBpm/d1:Value")
    )
    result$cad <- as.integer(
      extract_metainfo(filename, trackpoints, "ns3:RunCadence")
    )
  }

  attr(result, "filename") <- filename

  attr(result, "time") <-
    ymd_hms(xml_text(xml_find_first(xmldoc, ".//d1:Id")))

  attr(result, "type") <-
    tolower(trimws(xml_attr(xml_find_first(xmldoc, ".//d1:Activity"), "Sport")))

  new_act_tbl(result)
}

#' Extracts a given additional piece of metainformation from the trackpoints
#' @param filename The name of the file being parsed
#' @param trackpoints a vector of trackpoints, as from
#'                    `xml_find_all(xmldoc, ".//d1:trkpt")`
#' @param tagname The name of the tag to look for and extract
#' @importFrom xml2 xml_add_child xml_find_all xml_text
#'
#' @noRd
extract_metainfo <- function(filename, trackpoints, tagname) {
  xml_text(xml_find_first(trackpoints, paste0(".//", tagname)))
}
