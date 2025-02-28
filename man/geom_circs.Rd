% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsoap_svg.R
\name{geom_circs}
\alias{geom_circs}
\title{A function to convert all circles in a plot to a ggproto object for
interactive plotting.}
\usage{
geom_circs(onerow, geom.map)
}
\description{
This helper function is necessary for .svg file exportation. "ggforce",
a dependency package will output circles that disregards the row order of
the data frame. This implies that the .svg file produced either through
ggiraph or the base export function from RStudio will have tags that are
unable to be reassigned with the correct order of data.
}
