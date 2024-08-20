#' Prediction
#'
#' Predict data with the created models
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
    mnote,
    AOA = TRUE){

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

    modname <- paste0(
      mnote,
      "_",
      mod_df[i, ]$sensor,
      "_",
      dates[i],
      "_",
      mod_df[i, ]$modeltype,
      "_",
      mod_df[i, ]$classifier
    )
    mod <- readRDS(
      file.path(
        envrmt$path_models,
        paste0(modname,
               "_ffs_model.rds")
      )
    )

    print(
      paste0(
        "Making ",
        mod_df[i, ]$sensor,
        " ",
        mod_df[i, ]$classifier,
        "-prediction for ",
        dates[i],
        "."
        )
      )

    pred <- terra::predict(
      raster,
      mod,
      na.rm = TRUE
      )
    names(pred) <- modname
    terra::writeRaster(
      pred,
      file.path(
        envrmt$path_predictions,
        paste0(
          modname,
          "_prediction.tif"
          )
        ),
      overwrite = TRUE
    )

    if (isTRUE(AOA)) try({
      aoa <- CAST::aoa(
        raster,
        mod)
      names(aoa) <- paste0(modname, "_aoa")
      terra::writeRaster(
        aoa,
        file.path(
          envrmt$path_aoa,
          paste0(
            modname,
            "_aoa.tif"
          )
        ),
        overwrite = TRUE
      )
    })
  } # end i loop

  for (i in 1:nrow(mod_df)){
    mod_df$variables[[i]] <- paste(
      mod_df$variables[[i]],
      collapse = ", "
      )
  }
  mod_df <- data.frame(mod_df)
  mod_df <- apply(mod_df, 2, as.character)

  write.csv(
    mod_df,
    file.path(
      envrmt$path_statistics,
      paste0(
        mnote,
        "_best_models.csv"
      )
    ),
    row.names = FALSE
  )
} # end function loop
