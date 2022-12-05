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
predict <- function(fold = "LTO",
                    na.rm = F,
                    ...){
  model_list <- list();
  all_modells <- list.files(path = envrmt$path_ffsmodels, recursive = T);
  number_of_models <- length(all_modells);
  terraStack <- terra::rast(envrmt$path_rfinal, "allvars_rast.tif")
  eval <- readRDS(envrmt$path_statistics, paste0(fold, "_eval_df.rds"))

  for (i in number_of_models){
    model <- readRDS(all_modells[[i]])
    prediction <- raster::predict(terraStack, model, na.rm = na.rm)
    terra::writeRaster(prediction, file.path(envrmt$path_ffspredictions, paste0(fold, "_pred_", eval$sensor[i], "_", eval$year_month[i], ".grd")), overwrite = TRUE)
  }
}
