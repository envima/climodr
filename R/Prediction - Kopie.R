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
predict <- function(fold = "LLO",
                    ...){
  all_modells <- list.files(path = envrmt$path_models, recursive = F);
  number_of_models <- length(all_modells);
  terraStack <- terra::rast(file.path(envrmt$path_wraster, "2018_testdata_layer.tif"))
  #eval <- readRDS(envrmt$path_statistics, paste0(fold, "_eval_df.rds"))

  for (i in number_of_models){
    model <- readRDS(file.path(envrmt$path_models, all_modells[[i]]))
    prediction <- terra::predict(terraStack, model)
    #terra::writeRaster(prediction, file.path(envrmt$path_ffspredictions, paste0(fold, "_pred_", eval$sensor[i], "_", eval$year_month[i], ".grd")), overwrite = TRUE)
    terra::writeRaster(prediction, file.path(envrmt$path_predictions, paste0(fold, "_pred_", "_", i, ".grd")), overwrite = TRUE)
  }
}

predict <- function(time = NULL,
                    ...){
  if (time == forcem){
    #select month
    for (u in 1:12)try({
      #u = 1
      #r = 1
      mth <- u
      #overwrite layer to original
      #lany <- terra::rast()
      print(u)
      #loop over the df of vrt files
      for (r in 1:nrow(vars_yf))try({
        #read selected files
        preds = terra::rast(vars_yf[r,])
        #crop the climatic data
        b_vrt = terra::crop(preds[[mth]], b_dem)### find rightextnet file
        #adjust layer name
        names(b_vrt) = substring(vars_yf[r,],78,80)###you made the path longer beacuse you switched the directory....
        # #add all layers to prediction stack
        # add(lany) = b_vrt
        #write pretty names
        seq=gsub(".*/", "", mod_df[v,])
        n.mod=str_extract(seq, "^.*(?=(_ffs_model.rds))")#everything excluding the back
        #write pred stack as tif
        #writeRaster(layer,paste0(save,n.mod,"_","layer.tif"),overwrite=TRUE)
        #merge dem and spectral layers
        lany=c(b_vrt,layer)
        #names(layer)
        remove(b_vrt,preds)
        gc()
      })#close loop of stack spectral vars### ready layer stack

      #make prediction
      prediction = terra::predict(lany, rds, na.rm = T)
      #adjust layer name
      names(prediction)=paste0(n.mod,"_",u,"_pred")
      print(names(prediction))
      #save prediction as tif
      terra::writeRaster(prediction,paste0(save,n.mod,"_",u,"_pred.tif"),overwrite=TRUE)
      #chnage to raster
      layer2=brick(lany)
      # #do an AOA with parellelization
      cr=detectCores()
      cl <- makeCluster(cr-1)
      registerDoParallel(cl)
      aoa= CAST::aoa(layer2,rds)
      stopCluster(cl)##,e_rmse$year_month[v],"_",e_rmse$sensor[v]
      t_aoa=rast(aoa$AOA)
      terra::writeRaster(t_aoa$AOA,paste0(save,n.mod,"_",u,"_AOA.tif"),overwrite=TRUE)
      remove(prediction,t_aoa,lany,layer2,aoa)
      gc()
    }) ##close loop on monthly selection of spectral layer
  }
}

climpred <- function(fold = NULL,
                    AOA = TRUE,
                    ...){
  all_modells <- list.files(path = envrmt$path_models, recursive = T);
  number_of_models <- length(all_modells);
  lany <- terra::rast(file.path(envrmt$path_wraster, "2018_testdata_layer.tif"))
  #eval <- readRDS(envrmt$path_statistics, paste0(fold, "_eval_df.rds"))

  for (i in 1:number_of_models){
    rds <- readRDS(file.path(envrmt$path_models, all_modells[[i]]))
    n.mod <- stringr::str_extract(all_modells[[i]], "^.*(?=(_ffs_model.rds))")

    #make prediction
    prediction <- terra::predict(lany, rds, na.rm = T)

    #adjust layer name
    names(prediction) <- paste0(n.mod, "_", i,"_pred")
    print(names(prediction))

    #save prediction as tif
    terra::writeRaster(prediction, file.path(envrmt$path_predictions,
                                             paste0(n.mod, "_", i, "_pred.tif"),
                                             overwrite=TRUE)
                       )
    # do an AOA with parallelization
    if (AOA == TRUE){
      #change to raster
      layer2 = brick(lany)

      # initiate parallelization
      cr <- detectCores()
      cl <- makeCluster(cr - 2)
      registerDoParallel(cl)

      # calculate AOA
      aoa <- CAST::aoa(layer2, rds)
      stopCluster(cl) #,e_rmse$year_month[v],"_",e_rmse$sensor[v]

      # save AOA
      t_aoa <- terra::rast(aoa$AOA)
      terra::writeRaster(t_aoa$AOA, file.path(envrmt$path_predictions,
                                              paste0(n.mod, "_", u, "_AOA.tif"),
                                              overwrite=TRUE)
                         )
      remove(prediction, t_aoa,layer2, aoa)
      gc()
    } else {
      remove(prediction)
      gc()
    }
  }
}
