% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral_indices.r
\name{MTVI1}
\alias{MTVI1}
\title{Modified Triangular Vegetation Index 1}
\usage{
MTVI1(df, outp_fname = NULL, ...)
}
\arguments{
\item{df}{A data frame where columns represent measurements in a single wavelength,
and columns are named following Quantalab's conventions}

\item{outp_fname}{In case the input is raster data, this is the optional output filename to write the result to}

\item{...}{Arguments to be passed to get_band_of_wavelength, particularly band_txt, splitter, and i.}
}
\value{
A vector with the value of the index
}
\description{
Calculate Modified Triangular Vegetation Index
}
\references{
Haboudane et al 2004
}
