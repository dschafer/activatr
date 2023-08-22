#' activatr: Utilities for Parsing and Plotting Activities
#'
#' This contains helpful functions for parsing, managing, plotting, and
#' visualizing activities, most often from GPX (GPS Exchange Format) files
#' recorded by GPS devices. It allows easy parsing of the source files into
#' standard R data formats, along with functions to compute derived data for
#' the activity, and to plot the activity in a variety of ways.
#'
#' @docType package
#' @name activatr
#' @keywords internal
"_PACKAGE"

release_questions <- function() {
  c(
    "Have you run lintr::lint_package()?",
    "Have you run covr::report()?",
    "Have you run devtools::spell_check()?",
    "Have you run styler::style_pkg()?",
    "Have you run devtools::build_readme()?"
  )
}
