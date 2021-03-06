% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_sicktree_model.r
\name{run_sicktree_model}
\alias{run_sicktree_model}
\title{Run a distribution model using a raster brick as predictors}
\usage{
run_sicktree_model(tile, vuln_classes, model_dir, outp_dir)
}
\arguments{
\item{tile}{path to the raster where we want to run the model}

\item{vuln_classes}{A list of the classes you want to run existing models for
Only the fist place in each element will be used to search for existing models.}

\item{model_dir}{The folder and filename prefix to where the models were saved by \code{\link{calibrate_sicktree_model}}}

\item{outp_dir}{The folder and filename prefix to save the model output to}
}
\value{
Saves rasters predicted by the model
}
\description{
For each class in .shp polygon file, run an existing, calibrated, distribution model using a raster brick as predictors
}
\note{
Run in 32-bit R installation. Implement optional parallel
}
\examples{
\dontrun{
}
#' Laura

}
