#' Modelling
#'
#' Creates Models for each climate value
#'
#'
#' @return tiff, rds
#' @seealso
#'
#' @name predict
#' @export predict
#'
#' @examples
#'
climpred <- function(){
# read in dem
  b_dem <- terra::rast(file.path(envrmt$path_wraster, "dem_layer.tif"))

# read in all models
  mod_list <- as.data.frame(list.files(path = envrmt$path_models, pattern = "ffs_model.rds", recursive = TRUE))
  colnames(mod_list)[1] <- "mod"

# extract the best models, measured by rmse
#  finmod <- list.files(envrmt$path_models ,pattern = "best_rmse",full.names = TRUE)
# normal
#  e_rmse <- readRDS(finmod[1])

# create empty model data frame
#  mod_df <- data.frame()

# create a list with all raster images
  tiff_list <- list.files(path = envrmt$path_wraster, pattern = "testdata_layer.tif", recursive = TRUE)

  for (i in 1:lenght(tiff_list)){
    rasterStack <- terra::rast(file.path(envrmt$path_wraster, tiff_list[i]))
    terra::add(rasterStack) <- b_dem

    for (j in 1:length(mod_list)){
      mod <- readRDS(file.path(envrmt$path_models, mod_list[[j]]))

# write pretty names
      seq = gsub(".*/", "", mod_list[[j]])
      n.mod = stringr::str_extract(seq, "^.*(?=(_ffs_model.rds))") # everything excluding the back

# make prediction
      prediction = terra::predict(rasterStack, mod, na.rm = T)
# adjust layer name
      names(prediction) = paste0(n.mod,"_anl_pred")
      print(names(prediction))
# save prediction as tif
      terra::writeRaster(prediction,
                         file.path(envrmt$path_predictions,
                                   paste0(n.mod,"_anl_pred.tif")),
                         overwrite=TRUE)
# do an AOA
      cr <- parallel::detectCores()
      cl <- parallel::makeCluster(cr - 2)
      doParallel::registerDoParallel(cl)
      aoa <- CAST::aoa(rasterStack, mod)
      parallel::stopCluster(cl)

      terra::writeRaster(aoa$AOA,
                         file.path(envrmt$path_predictions, paste0(n.mod,"_anl_aoa.tif")),
                         overwrite = TRUE)
    } # end prediction loop
  } # end raster loop
} # end function loop
