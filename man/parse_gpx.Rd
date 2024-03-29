% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_gpx}
\alias{parse_gpx}
\title{Parses a GPX file into a \code{\link[=act_tbl-class]{act_tbl}}}
\usage{
parse_gpx(filename, detail = c("basic", "latlon", "advanced"), every = NA)
}
\arguments{
\item{filename}{The GPX file to parse}

\item{detail}{How much detail to parse from the GPX.
\itemize{
\item If \code{basic} (the default), this will parse \code{lat} / \code{lon} /
\code{ele} / \code{time} columns.
\item If \code{latlon}, this will only parse \code{lat}/\code{lon}. This is particularly
useful for GPX files exported without time information, such as from
Strava.
\item If \code{advanced}, it will load everything from basic, plus \code{hr} /
\code{cad}. This is most useful for files that have heart rate and cadence
information.
}}

\item{every}{Optional. If provided, determines how frequently points will
be sampled from the file, so if 10 is provided, every tenth point will
be selected. If omitted or set to 1, every point will be selected. Must
be a positive integer.

This is most useful to quickly analyze a large file, since parsing is much
faster when skipping 90\% of the data points.}
}
\value{
A \code{\link[=act_tbl-class]{act_tbl}} with one row for each trackpoint in the
.  GPX (modified by \code{every}), and with the columns determined by \code{detail}.

\item{lat}{Latitude, a double in degrees between -90 and 90.}
\item{lon}{Longitude, a double in degrees between -180 and 180.}
\item{ele}{Elevation, a double in meters.}
\item{time}{A date-time representing the time of the point.}
\item{hr}{Heart rate, an int in beats per minute.}
\item{cad}{Cadence, an int in one-foot steps per minute.}

Additionally, attributes are set on the returned object containing top level
data from the GPX. Each of these will be NA when not provided in the file.

\item{filename}{The filename this was parsed from, a string. This is always
present, and is always the value of the \code{filename}
parameter.}
\item{time}{A date-time representing the time of the activity.}
\item{title}{A string.}
\item{desc}{A string.}
\item{type}{A string.}
}
\description{
This parses a standard GPS Exchange Format XML (GPX) file into an
data frame with class \code{\link[=act_tbl-class]{act_tbl}}. See \code{vignette("parsing")}
for examples.
}
\examples{
example_gpx_file <- system.file(
  "extdata",
  "running_example.gpx.gz",
  package = "activatr"
)
act_tbl <- parse_gpx(example_gpx_file)
print(act_tbl, n = 5)
attr(act_tbl, "title")

nrow(parse_gpx(example_gpx_file))
nrow(parse_gpx(example_gpx_file, every = 100))

colnames(parse_gpx(example_gpx_file))
colnames(parse_gpx(example_gpx_file, detail = "latlon"))
colnames(parse_gpx(example_gpx_file, detail = "advanced"))
}
\seealso{
\url{https://en.wikipedia.org/wiki/GPS_Exchange_Format}

\url{https://www.topografix.com/gpx.asp}
}
