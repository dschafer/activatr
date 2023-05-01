#' Parses a GPX file into a tibble.
#'
#' This parses a standard GPS Exchange Format XML (GPX) file into an act_tbl.
#'
#' @param filename The GPX file to parse
#' @param detail How much detail to parse from the GPX.
#'   * If "basic", the default, this will load \code{lat} / \code{lon} /
#'   \code{ele} / \code{time}.
#'   * If "latlon", it will only load \code{lat}/\code{lon}: useful for GPX
#'   files exported without time information.
#'   * If "advanced", it will load everything from basic, plus \code{hr} /
#'   \code{cad} / \code{atemp}: useful for files with HR information.
#' @param every Optional. If provided, determines how frequently points will
#'   be sampled from the file, so if 10 is provided, every tenth point will
#'   be selected. If omitted or set to 1, every point will be selected. Must
#'   be a positive integer.
#' @return A \code{act_tbl} with one row for each trackpoint in the GPX
#'   (modified by \code{every}), and with the columns determined by
#'   \code{detail}.
#'
#'     \item{lat}{latitude, a dbl in degrees between -90 and 90}
#'     \item{lon}{longitude, a dbl in degrees between -180 and 180}
#'     \item{ele}{elevation, a dbl in meters}
#'     \item{time}{time, a dttm representing the time of the point}
#'     \item{hr}{heart rate, an int in beats per minute}
#'     \item{cad}{cadence, an int in one-foot steps per minute}
#'
#'  Additionally, attributes are set on the tibble containing top level data
#'  from the GPX. Each of these will be NA when not provided in the file.
#'
#'     \item{filename}{the filename this was parsed from. This is always
#'                     present, and is always the value of the \code{filename}
#'                     argument.}
#'     \item{time}{time, a dttm representing the time of the GPX}
#'     \item{title}{title, a chr}
#'     \item{desc}{description, a chr}
#'     \item{type}{type, a chr}
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_find_first
#' @importFrom lubridate ymd_hms
#' @importFrom rlang abort
#' @importFrom tibble tibble
#'
#' @export
#'
#' @seealso https://en.wikipedia.org/wiki/GPS_Exchange_Format
#' @seealso https://www.topografix.com/gpx.asp
#'
#' @examples
#' running_file <- system.file(
#'   "extdata",
#'   "running_example.gpx.gz",
#'   package = "activatr"
#' )
#' running_df <- parse_gpx(running_file)
parse_gpx <- function(filename,
                      detail = c("basic", "latlon", "advanced"),
                      every = NA) {
  xmldoc <- read_xml(filename)

  if (is.na(every) || every == 1) {
    trackpoints <- xml_find_all(xmldoc, ".//d1:trkpt")
  } else if (!is.numeric(every) ||
    every < 1 ||
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

#' Parses a TCX file into a tibble.
#'
#' This parses a standard Training Center XML (TCX) file into an \code{act_tbl}.
#'
#' @param filename The TCX file to parse
#' @param detail How much detail to parse from the TCX.
#'   * If "basic", the default, this will load \code{lat} / \code{lon} /
#'   \code{ele} / \code{time}.
#'   * If "latlon", it will only load \code{lat}/\code{lon}: useful for TCX
#'   files exported without time information.
#'   * If "advanced", it will load everything from basic, plus \code{hr} /
#'   \code{cad} / \code{atemp}: useful for files with HR information.
#' @param every Optional. If provided, determines how frequently points will
#'   be sampled from the file, so if 10 is provided, every tenth point will
#'   be selected. If omitted or set to 1, every point will be selected. Must
#'   be a positive integer.
#' @return A \code{act_tbl} with one row for each trackpoint in the TCX
#'   (modified by \code{every}), and with the columns determined by
#'   \code{detail}.
#'
#'     \item{lat}{latitude, a dbl in degrees between -90 and 90}
#'     \item{lon}{longitude, a dbl in degrees between -180 and 180}
#'     \item{ele}{elevation, a dbl in meters}
#'     \item{time}{time, a dttm representing the time of the point}
#'     \item{hr}{heart rate, an int in beats per minute}
#'     \item{cad}{cadence, an int in one-foot steps per minute}
#'
#'  Additionally, attributes are set on the tibble containing top level data
#'  from the TCX. Each of these will be NA when not provided in the file.
#'
#'     \item{filename}{the filename this was parsed from. This is always present
#'                     is always the value of the \code{filename} argument.}
#'     \item{time}{time, a dttm representing the time of the TCX}
#'     \item{type}{type, a chr}
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_find_first
#' @importFrom lubridate ymd_hms
#' @importFrom rlang abort
#' @importFrom tibble tibble
#'
#' @export
#'
#' @seealso https://en.wikipedia.org/wiki/Training_Center_XML
#'
#' @examples
#' running_file <- system.file(
#'   "extdata",
#'   "running_example.tcx.gz",
#'   package = "activatr"
#' )
#' running_df <- parse_gpx(running_file)
parse_tcx <- function(filename,
                      detail = c("basic", "latlon", "advanced"),
                      every = NA) {
  xmldoc <- read_xml(filename)

  if (is.na(every) || every == 1) {
    trackpoints <- xml_find_all(xmldoc, ".//d1:Trackpoint[d1:Position]")
  } else if (!is.numeric(every) ||
    every < 1 ||
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
#'                    \code{xml_find_all(xmldoc, ".//d1:trkpt")}
#' @param tagname The name of the tag to look for and extract
#' @importFrom magrittr %>%
#' @importFrom xml2 xml_add_child xml_find_all xml_text
#'
#' @noRd
extract_metainfo <- function(filename, trackpoints, tagname) {
  xml_text(xml_find_first(trackpoints, paste0(".//", tagname)))
}
