% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shp_copy.r
\name{shp_copy}
\alias{shp_copy}
\title{Copy shapefiles between directories}
\usage{
shp_copy(from, to, overwrite = F)
}
\arguments{
\item{from}{Character vector containing filenames of the .shp file to be copied}

\item{to}{Character vector containing filenames of the .shp file to be copied}

\item{overwrite}{logical. Should existing destination files be overwritten?}
}
\value{
A logical indicating whether the copying action was successful.
}
\description{
Copy shapefiles, ensuring that all the supporting files (.shx, .qpj, .dbf, .xml, .prj) get copied too
}
\examples{
\dontrun{
# providing a destination filename
shp_copy(from = "C:/Users/pieterbeck/Documents/simple_outline.shp",
to = "C:/Users/pieterbeck/Documents/temp/my_backup.shp")

# providing only a destination directory
shp_copy(from = "C:/Users/pieterbeck/Documents/simple_outline.shp",
to = "C:/Users/pieterbeck/Documents/temp/")
}
}
