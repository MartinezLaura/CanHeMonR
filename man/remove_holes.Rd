% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/region_growing_functions.r
\name{remove_holes}
\alias{remove_holes}
\title{Close Holes in Polygons}
\usage{
remove_holes(SpatPoly)
}
\arguments{
\item{SpatPoly}{A SpatialPolygons object}
}
\value{
the SpatialPolygons object with any holes removed
}
\description{
Given an object of class Polygons, return #a similar object with no holes
}
\note{
called in region_growing_wrapper.r
}
\references{
http://r-sig-geo.2731867.n2.nabble.com/Remove-holes-from-a-SpatialPolygon-td7585464.html
}
