% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate_sicktree_model_multi_tile.r
\name{calibrate_sicktree_model_multi_tile}
\alias{calibrate_sicktree_model_multi_tile}
\title{Calibrate vegetation distribution models}
\usage{
calibrate_sicktree_model_multi_tile(vuln_classes = "ALL", training_df,
  model_outp_dir)
}
\arguments{
\item{vuln_classes}{A character vector of the classes you want to model. The should be presented in the column 'class' of training_df.}

\item{training_df}{data.frame, with in the column 'pres' 1/0 to indicate presence absence, then covariate columns, and a colum 'class' groupin grows by
the land-cover class the data was sampled for. This df is typically generated by sample_for_sicktree_model_multi_tile}

\item{model_outp_dir}{The folder and filename prefix to save the model objects to}
}
\value{
Saves class-specific distribution models, using a data frame created from training points and covariate images
}
\description{
For each class in .shp polygon file, calibrate a distribution model using a raster brick as predictors
}
\note{
Run in 32-bit R installation. Do you need a 'require(rJava)?'. Implement optional parallel
}
\examples{
\dontrun{
#calibrate the model with 10 absences per tile for Pbclass
}


}
