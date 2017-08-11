#' @title Create a tx file with predictors
#' @description Create a txt file with the paths of the predictors to be used in the run_sicktree_model
#' @param r_train_dir A directory where .tifs for the prediction can be found
#' @param text_train_dir A directory where .tifs of the textures associated with r_train_dir
#' @param tile Character vector. Names of tile(s) to run. 'ALL will run all tiles in r_train_dir. Default is 'ALL'
#' @param ninputs_tile Number of inputs that we have fore each tile, for exemple number of textures
#' @param data_outp_dir The folder and filename prefix to save the sampled data to. No data is saved is data_outp_dir is NULL. Default is NULL.
#' @param parallel Should the code be run in parallel using the doParallel package? Default is FALSE.
#' @param nWorkers If running the ocde in parallel, how many workers should be used? Default is 4.
#' @return A txt file with the paths of the tiles to predict in the function run_sicktree_model_multi_tile.r
#' @examples \dontrun{
# tt <- make_predictor_txt_files(r_train_dir <-'/H03_CANHEMON/Imagery/Portugal/ADS100/ortophotos_06032017/geotif',
#                                text_train_dir <-'/home/martlur/Documents/TexturesAds/',
#                                tile <- 'ALL', ninputs_tile = 27, data_outp_dir = "/DATA/Results/Rcode/Predictors/",
#                                parallel <- F, nWorkers = 4)
#'}
#' @export
make_predictor_txt_files <- function(r_train_dir,
                                     text_train_dir,
                                     tile,
                                     ninputs_tile,
                                     data_outp_dir,
                                     parallel,
                                     nWorkers){

  #harvest all the tif files in the directories holding covariate/predictor images
  all_tifs <- list.files(r_train_dir, recursive = T, full.names = T, pattern = ".tif")
  all_tifs <- append(all_tifs, list.files(text_train_dir, recursive = T, full.names = T, pattern = ".tif"))
  # all_tifs <- all_tifs[grepl('.tif',all_tifs)]
  #excluded the tif files in the unprojected folder
  all_tifs <- all_tifs[!grepl('orig_noPRJ', all_tifs)]

  #if you want to run all the tiles in a directory, harvest the available tilenames
  if (tile[1] == 'ALL'){
    tile <- substr(basename(all_tifs),1,16)
    tile <- unique(tile)
    #only keep tiles that start  with 'pt'
    tile <- tile[substr(tile,1,2) == 'pt']
    #only keep the original tiles and avoid to duplicate the basename. Ex: pt617000_4404000 and pt617000-4404000
    #only happends when the textures and the tiles are in the same folder
    tile <- tile[substr(tile,9,9) == '-']
    cat(length(tile),' tiles are considered\n')
  }

  tile_counter = length(tile)


  #set up the cluster for parallel processing
  if (parallel){
    try(parallel::stopCluster(cl), silent=T)
    # Test that the cores assign not overlap the maximum cores available
    maxcl <- (parallel::detectCores(logical = TRUE)-1)
    if (nWorkers <= maxcl){
      cl <- parallel::makeCluster(nWorkers)
      doParallel::registerDoParallel(cl)
    }else{
      cl <- parallel::makeCluster(maxcl)
      doParallel::registerDoParallel(cl)
    }
  }

  #choose the appropriate operator for the foreach loop
  `%op%` <- if (parallel) `%dopar%` else `%do%`

  stime <- system.time({predictors <- foreach::foreach(i = 1:length(tile), .combine = rbind.data.frame, .inorder=F, .multicombine=F, .errorhandling='remove') %op% {
    tile_i <- tile[i]

   #browser()
      #make alternative tile code (Margherita uses these in the texture filenames)
      tile_i_multiversion <- unique(c(tile_i, gsub('_','-',tile_i),gsub('-','_',tile_i),gsub('-','\\.',tile_i),gsub('_','\\.',tile_i),gsub('\\.','-',tile_i),gsub('\\.','_',tile_i)))
      tile_i_multiversion_for_regexpr <- paste(tile_i_multiversion, collapse = "|")
      pred_rs <- all_tifs[grepl(tile_i_multiversion_for_regexpr, all_tifs)]

      #an empty data frame to hold the data extracted for this tile
      if (length(pred_rs) == ninputs_tile){
        #check if you have any points in this tile
        txt_name <- paste0('predictors_', gsub('-','_',tile_i), '.txt')
        pred_names <- gsub(paste0(r_train_dir,"/"), "",pred_rs)
        write.table(pred_names, file.path(data_outp_dir,txt_name),col.names = F, row.names = F)
      }
      else{
        cat("Not enoght layers for tile:", tile_i,"\n")
        tile_counter = tile_counter-1
      }

      #empty the garbage collector after each iteration
      gc()
  }
  })
  cat('Created of predictors of',tile_counter,' tiles in ',round(stime/60),' minutes\n')
  return(predictors)
}
