% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_gpx}
\alias{parse_gpx}
\title{Parses a GPX file into a tibble.}
\usage{
parse_gpx(filename, detail = c("basic", "latlon", "advanced"), every = NA)
}
\arguments{
\item{filename}{The GPX file to parse}

\item{detail}{How much detail to parse from the GPX.
* If "basic", the default, this will load \code{lat} / \code{lon} /
\code{ele} / \code{time}.
* If "latlon", it will only load \code{lat}/\code{lon}: useful for GPX
files exported without time information.
* If "advanced", it will load everything from basic, plus \code{hr} /
\code{cad} / \code{atemp}: useful for files with HR information.}

\item{every}{Optional. If provided, determines how frequently points will
be sampled from the file, so if 10 is provided, every tenth point will
be selected. If omitted or set to 1, every point will be selected. Must
be a positive integer.}
}
\value{
A \code{act_tbl} with one row for each trackpoint in the GPX
  (modified by \code{every}), and with the columns determined by
  \code{detail}.

    \item{lat}{latitude, a dbl in degrees between -90 and 90}
    \item{lon}{longitude, a dbl in degrees between -180 and 180}
    \item{ele}{elevation, a dbl in meters}
    \item{time}{time, a dttm representing the time of the point}
    \item{hr}{heart rate, an int in beats per minute}
    \item{cad}{cadence, an int in one-foot steps per minute}

 Additionally, attributes are set on the tibble containing top level data
 from the GPX. Each of these will be NA when not provided in the file.

    \item{filename}{the filename this was parsed from. This is always
                    present, and is always the value of the \code{filename}
                    argument.}
    \item{time}{time, a dttm representing the time of the GPX}
    \item{title}{title, a chr}
    \item{desc}{description, a chr}
    \item{type}{type, a chr}
}
\description{
This parses a standard GPS Exchange Format XML (GPX) file into an act_tbl.
}
\examples{
running_file <- system.file(
  "extdata",
  "running_example.gpx.gz",
  package = "activatr"
)
running_df <- parse_gpx(running_file)
}
\seealso{
https://en.wikipedia.org/wiki/GPS_Exchange_Format

https://www.topografix.com/gpx.asp
}
