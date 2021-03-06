% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shift_polygons_to_points.r
\name{shift_polygons_to_points}
\alias{shift_polygons_to_points}
\title{Shift Polygons To Points}
\usage{
shift_polygons_to_points(polygon_shp, source_point_shp = NA, target_point_shp,
  outp_fname, extent_to_process = NA, sample_size_for_model = NA,
  attributes_to_retain = NA)
}
\arguments{
\item{polygon_shp}{Filename of the polygon shapefile to reproject}

\item{source_point_shp}{Optional filename of the point shapefile that contains points at the source locations of the polygons, to calibrate the reprojection.
If a polygon shapefile is provided, its centroids will be used.
If source_point_shp is NA (the default), the centroids of polygon_shp will be used to calibrate the reprojection.}

\item{target_point_shp}{Filename of the point shapefile that contain the points at the target locations for the polygons. E.g. the centroids of a segmentation operation}

\item{outp_fname}{Filename of the output shapefile. Both need to have common projection!}

\item{extent_to_process}{Extent object to which both data sets will be cropped before processing. Default is NA, i.e. no cropping}

\item{sample_size_for_model}{Integer. How many data points should be sampled to construct the tps model. Default is NA, i.e. all samples are used.}

\item{attributes_to_retain}{Character vector. If polygon_shp is a SpatialPolygonsDataFrame, chosen attributes can be retained in the output.
Options are: NA (default, not attributes are retained), Inf (all attributes are retained), or a vector with particular attributes}
}
\description{
Spatially shift Spatial Polygons (e.g. crowns delineated in image A) to be centered over Spatial Points (e.g. centroids of crown-like segments
identified in image B)
}
\details{
In some cases the polygons to reproject are not the entire crowns, and their centroids are not representative of the crown center.
In these cases it is useful to provide source_point_shp to use the entire crown to create the reprojection model. This is the case, for example,
when the crowns to be reprojected for overlay on thermal imagery, have been clipped to retain only the highest quantiles in 800 nm reflectance, using assing_crowncells_quantile.
}
\examples{
\dontrun{
workdir <- 'H:/FISE/forest/CanopyHealthMonitoring/PWN/flights_final/'
polygon_shp <- 'E:/beckpie/temp/trimmed.shp'
source_point_shp <- file.path(workdir,'/shp+stats/150727/150727_mca/crown_150727_mca_all_trees_99.shp')

wd <- 'H:/FISE/forest/CanopyHealthMonitoring/PWN/flights_final/shp+stats/150727/150727_flir'
target_point_shp <- file.path(wd,'indices_centroide2.shp')
outp_fname <- file.path(wd,'crown_150727_flir_all_trees_99_tps.shp')
extent_to_process <- raster::extent(c(747287,748000,4463535,4464600))
require(CanHeMonR)
shift_polygons_to_points(polygon_shp = polygon_shp, source_point_shp = source_point_shp,
                         target_point_shp = target_point_shp, outp_fname = outp_fname, extent_to_process = extent_to_process, attributes_to_retain = c('primalX' ,'primalY'))
}
}
