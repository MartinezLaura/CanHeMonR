#' @title Calibrate vegetation distribution models
#' @description For each class in .shp polygon file, calibrate a distribution model using a raster brick as predictors
#' @param vuln_classes A character vector of the classes you want to model. The should be presented in the column 'class' of training_df.
#' @param training_df data.frame, with in the column 'pres' 1/0 to indicate presence absence, then covariate columns, and a colum 'class' groupin grows by
#' the land-cover class the data was sampled for. This df is typically generated by sample_for_sicktree_model_multi_tile
#' @param model_outp_dir The folder and filename prefix to save the model objects to
#' @note Run in 32-bit R installation. Do you need a 'require(rJava)?'. Implement optional parallel
#' @return Saves class-specific distribution models, using a data frame created from training points and covariate images
#' @examples \dontrun{
#' #calibrate the model with 10 absences per tile for Pbclass
# tt <- calibrate_sicktree_model_multi_tile (vuln_classes = list(c('Pb')), training_df = readRDS('/media/laura/Laura/Rcode/Sicktree/maxent_training_dfs.rdsdata'),
#                                                   model_outp_dir = paste0('/media/laura/Laura/Rcode/Sicktree/','samp100_'))
#' }


#'
#'
#' @export
calibrate_sicktree_model_multi_tile <- function(vuln_classes = 'ALL', training_df, model_outp_dir){
  if (R.Version()$arch != "i386"){
    #If  64bit version, deactivate JAVA_HOME it within your R-session with the following code before loading rJava
    if (Sys.getenv("JAVA_HOME")!="")
      Sys.setenv(JAVA_HOME="")
    library(rJava)
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++
  #run in R 32 bit
  #see http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
  #http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf

  require(maptools)
  #read in the image
  require(raster)
  require(dismo)
  require(rJava)

  #for each class that should be modelled
  if (vuln_classes == 'ALL'){
    vuln_classes <- unique(training_df$class)
  }

  for (class in vuln_classes){
    #+++++++++++++++++++++++++++++++++++++++++++++++
    # get the data for this class
    #+++++++++++++++++++++++++++++++++++++++++++++++

    class_rows <- which(training_df$class == class)
    class_resp <- training_df$pres[class_rows]
    class_pred <- within(training_df, rm(pres,class))[class_rows,]
    #+++++++++++++++++++++++++++++++++++++++++++++++
    # calibrate the model ----
    #+++++++++++++++++++++++++++++++++++++++++++++++
#browser()
    dismo::maxent()
    mod2 <- dismo::maxent(p = class_resp, x = class_pred)
    assign(paste0('mod.',class),mod2)

    #+++++++++++++++++++++++++++++++++++++++++++++++
    # save the model ----
    #+++++++++++++++++++++++++++++++++++++++++++++++
    model.file <- paste0(model_outp_dir,paste0(class,".rdsdata"))
    saveRDS(mod2,file=model.file)
    cat('Wrote away ',model.file,'\n')

  }
return(mod2)
}
