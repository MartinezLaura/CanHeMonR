#' @title Create a tx file with predictors
#' @description Create a txt file with the paths of the predictors to be used in the run_sicktree_model
#' @param r_train_dir A directory where .tifs for the prediction can be found
#' @param tile Character vector. Names of tile(s) to run. 'ALL will run all tiles in r_train_dir. Default is 'ALL'
#' @param data_outp_dir The folder and filename prefix to save the sampled data to. No data is saved is data_outp_dir is NULL. Default is NULL.
#' @param parallel Should the code be run in parallel using the doParallel package? Default is FALSE.
#' @param nWorkers If running the ocde in parallel, how many workers should be used? Default is 4.
#' @return A txt file with the paths of the tiles to predict in the function run_sicktree_model_multi_tile.r
#' @examples \dontrun{
# tt <- make_predictor_txt_files(r_train_dir <- "/media/laura/Laura/ADS100_06032017/Calibrate/",
#                                tile <- 'ALL', data_outp_dir = "/media/laura/Laura/Rcode/Sicktree/",
#                                parallel <- F, nWorkers = 4)
#'}

make_predictor_txt_files <- function(r_train_dir, tile, data_outp_dir, parallel, nWorkers){

  r_train_dir
  #harvest all the tif files in the directories holding covariate/predictor images
  all_tifs <- list.files(r_train_dir, recursive = T, full.names = T)
  all_tifs <- all_tifs[grepl('.tif',all_tifs)]
  #excluded the tif files in the unprojected folder
  all_tifs <- all_tifs[!grepl('orig_noPRJ', all_tifs)]

  #if you want to run all the tiles in a directory, harvest the available tilenames
  if (tile[1] == 'ALL'){
    tile <- substr(basename(all_tifs),1,16)
    tile <- unique(tile)
    #only keep tiles that start  with 'pt'
    tile <- tile[substr(tile,1,2) == 'pt']

    cat(length(tile),' tiles are considered\n')
  }

  tile_counter <- 0


  #set up the cluster for parallel processing
  if (parallel){
    try(parallel::stopCluster(cl), silent=T)
    # TO DO add a line that avoids allocating more workers than you have cores
    cl <- parallel::makeCluster(nWorkers)
    doParallel::registerDoParallel(cl)
  }
  #choose the appropriate operator for the foreach loop
  `%op%` <- if (parallel) `%dopar%` else `%do%`

  #stime <- system.time({
  #  maxent_training_dfs <- foreach::foreach(i = 1:length(tile), .combine = rbind.data.frame, .inorder=F, .multicombine=F, .errorhandling='remove') %op% {
    for (tile_i in tile){
   #browser()
      #make alternative tile code (Margherita uses these in the texture filenames)
      tile_i_multiversion <- unique(c(tile_i, gsub('_','-',tile_i),gsub('-','_',tile_i),gsub('-','\\.',tile_i),gsub('_','\\.',tile_i),gsub('\\.','-',tile_i),gsub('\\.','_',tile_i)))
      tile_i_multiversion_for_regexpr <- paste(tile_i_multiversion, collapse = "|")
      # pred_rs <- list.files(r_train_dir, recursive = T, full.names = T)
      #pred_rs <- pred_rs[grepl('.tif',pred_rs)]
      pred_rs <- all_tifs[grepl(tile_i_multiversion_for_regexpr, all_tifs)]

      #an empty data frame to hold the data extracted for this tile
      if (length(pred_rs) == 27){################################################### this is until the copying is complete!
        #check if you have any points in this tile
        txt_name <- paste0('predictors_', gsub('-','_',tile_i), '.txt')
        pred_names <- gsub(paste0(r_train_dir,"/"), "",pred_rs)
        write.table(pred_names, file.path(data_outp_dir,txt_name),col.names = F, row.names = F)
      }
    }
}
