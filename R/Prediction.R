#' Prediction
#'
#' Predictdata with the created models
#'
#'
#' @return
#' @seealso
#'
#' @name climpred
#' @export climpred
#'
#' @examples
#'

climpred <- function(
    method = "monthly",
    mnote){

  # create a list with all raster images
  tiff_list <- list.files(
    path = envrmt$path_rfinal,
    pattern = ".tif",
    recursive = TRUE
  )

  # read dgm
  dgm <- terra::rast(
    file.path(
      envrmt$path_rfinal,
      grep(
        pattern = "_dgm_",
        tiff_list,
        value = TRUE)
      )
    )


# read in all models
  mod_list <- list.files(
    path = envrmt$path_models,
    pattern = "_model.rds",
    recursive = TRUE
    )

# read eval_df
  eval_df <- readRDS(
    file.path(
      envrmt$path_statistics,
      paste0(
        mnote,
        "_mod_eval_df.rds"
        )
      )
    )

# filter for best models
  dates <- unique(eval_df[, 1])
  for (i in 1:length(dates)){
    mod_date <- eval_df[which(eval_df[, 1] == dates[i]), ]
    ifelse(
      i == 1,
      mod_df <- mod_date[
        which(mod_date$Nrmse == min(mod_date$Nrmse)), ],
      mod_df[i, ] <- mod_date[
        which(mod_date$Nrmse == min(mod_date$Nrmse)), ]
      )
# read fitting raster
    raster <- terra::rast(
      file.path(
        envrmt$path_rfinal,
        grep(
          pattern = dates[i],
          tiff_list,
          value = TRUE
        )
      )
    )
    terra::add(raster) <- dgm

    mod <- readRDS(
      file.path(
        envrmt$path_models,

      )
    )
  } # end i loop

    for (j in 1:nrow(mod_list)){
      mod <- readRDS(file.path(envrmt$path_models, mod_list[j, ]))

# write pretty names
      seq <- gsub(".*/", "", mod_list[j, ])
      n.mod <- stringr::str_extract(seq, "^.*(?=(_ffs_model.rds))") # everything excluding the back

# make prediction
      prediction <- terra::predict(rasterStack, mod, na.rm = T)
# adjust layer name
      names(prediction) <- paste0(n.mod, "_anl_pred")
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
                         file.path(envrmt$path_predictions, "aoa", paste0(n.mod, "_anl_aoa.tif")),
                         overwrite = TRUE)
    } # end prediction loop
} # end function loop
