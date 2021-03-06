% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_pnt_shp_files_by_attributes.r
\name{merge_pnt_shp_files_by_attributes}
\alias{merge_pnt_shp_files_by_attributes}
\title{Merge Point Shapefiles By Attributes}
\usage{
merge_pnt_shp_files_by_attributes(input_shps, outp_shp,
  common_attribs = c("primalX", "primalY"), attribs_to_drop = NA)
}
\arguments{
\item{input_shps}{Character vector with filenames of shapfiles to merge}

\item{outp_shp}{Filename of the output shapefile}

\item{common_attribs}{Character vector. The names of attributes that are present in each of the input_shps.
Most commonly it is of length 2 giving the attributes that indicate x and y coordinates.
These attributes must be present and equivalent in all input shapefiles. Default is c('primalX','primalY')}

\item{attribs_to_drop}{Optional. The names of any attributes you won't to drop from the output.}
}
\value{
Writes away a shapefile containing all attributes of the inputs. The attributes in the output have a 'i#' prefex referring
to the input it drew from. It also writes away a text file that links the ix numbering in the attribute names of the .shp back to the filenames
of the single-image files. The filename of the text file is outp_shp with '.txt' appended
}
\description{
...
}
