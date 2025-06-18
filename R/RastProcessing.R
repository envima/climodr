#' Cropping tiff data
#'
#' Crops input data to the extent size and reprojects them into project Coordinate reference system.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method character. Use "MB_Timeseries" for now. More methods are planned and will be added in future.
#' @param crs    Coordinate reference system Used to crop all images in folder_path. If crs
#'               it will automatically reprojected into this one. Default: crs of smallest Extent.
#' @param ext    SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#' @param overwrite logical. Should existing files with the same filename be
#'               overwritten? Default = FALSE
#' @param ...    arguments passed down from other functions.
#'
#' @return SpatRaster-Stack. Also saved to /workflow/rworkflow
#' @seealso `fin.csv`, `calc.indices`
#'
#' @name crop.all
#' @export crop.all
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' # Crop all raster bands
#' crop.all(envrmt = envrmt,
#'          method = "MB_Timeseries",
#'          overwrite = TRUE)
#'
crop.all <- function(envrmt = .GlobalEnv$envrmt,
                     method = "MB_Timeseries",
                     crs = NULL,
                     ext = NULL,
                     overwrite = FALSE,
                     ...) {
  all_files_in_distribution <- list.files(path = envrmt$path_raster, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep(".tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    message(paste0("Reading in raster ", i, "/", number_of_tiffs, "."))
    tif <- terra::rast(file.path(envrmt$path_raster, tiff_paths[[i]]))

    message("Cropping raster...")
    if (is.null(ext)){
      res_area <- terra::rast(file.path(envrmt$path_dep, "res_area.tif"))
      tif <- terra::project(tif, res_area)
      tif <- terra::mask(tif, res_area)
    } else {
      tif <- terra::project(tif, ext)
      try(tif <- terra::mask(tif, ext), silent = TRUE)
    }

    if (method == "Singlebands"){
      if (i == 1){
        tiff_stack <- tif
      }
      else {
        terra::add(tiff_stack) <- tif
      }

      if (!is.null(crs)){
        if (is.null(terra::crs(tiff_stack[[i]]))){
          message("Adding coordinate reference system to raster...")
          terra::crs(tiff_stack[[i]]) <- crs
        }
        else {
          if (!terra::crs(tiff_stack[[i]]) == crs){
            message("Reprojecting raster...")
            tiff_stack[[i]] <- terra::project(tiff_stack[[i]], crs)
          }
        }
      } # end crs loop
    } # end singleband loop

    if (method == "MB_Timeseries"){
      if (!is.null(crs)){
        if (is.null(terra::crs(tif))){
          message("Adding coordinate reference system to raster...")
          terra::crs(tif) <- crs
        } else {
          if (!terra::crs(tif) == crs){
            message("Reprojecting raster...")
            tif <- terra::project(tif, crs)
          }}
        } # end crs loop

      if(grepl("dgm", gsub(".{4}$", "", tiff_paths[i]), fixed = TRUE)){
        message(paste0("Saving DGM to rfinal [", envrmt$path_rfinal, "]."))
        terra::writeRaster(tif, file.path(envrmt$path_rfinal,
                                          paste0(gsub(".{4}$", "", tiff_paths[i]),
                                                 "_crop.tif")),
                           overwrite = overwrite)
      } else {
        message(paste0("Saving raster to rworkflow [", envrmt$path_rworkflow, "]."))
        terra::writeRaster(tif, file.path(envrmt$path_rworkflow,
                                          paste0(gsub(".{4}$", "", tiff_paths[i]),
                                                 "_crop.tif")),
                           overwrite = overwrite)
      }
    } # end MB_timeseries_loop
  };

  if (method == "Singleband"){
    return(tiff_stack);
    message(paste0("Saving rasterstack to rworkflow [", envrmt$path_rworkflow, "]."))
    terra::writeRaster(tiff_stack, file.path(envrmt$path_rworkflow, "tiff_stack.tif"), overwrite = overwrite)

  }; # end 2nd Singleband Loop
}

#' Calculate spectral indices
#'
#' Calculates a set of spectral indices to have more predictor variables available when further modeling.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param vi Character. Either "all" or vector containing the preferred spectral indices. See 'Details' for more information.
#' @param bands Character. Vector with lenght(bands) = 10. Contains the names of the bands in the Raster Stack.
#' If bands from the *Usage* example vector dont exist, use "NA" in their position. See 'Details' for more information.
#' @param overwrite logical. Argument passed down from `terra`-package. Overwrite existing files?
#'
#' @return SpatRaster-Stack
#' @seealso `crop.all`, `fin.csv`
#'
#' @name calc.indices
#' @export calc.indices
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' # Crop all raster bands
#' crop.all(envrmt = envrmt,
#'          method = "MB_Timeseries",
#'          overwrite = TRUE)
#'
#' # Calculate Indices from cropped raster bands
#' calc.indices(envrmt = envrmt,
#'              vi = "all",
#'              bands = c("blue", "green", "red",
#'                        "nir", "nirb",
#'                        "re1", "re2", "re3",
#'                        "swir1", "swir2"),
#'              overwrite = TRUE)
#'

calc.indices <- function(envrmt = .GlobalEnv$envrmt,
                         vi = "all",
                         bands = c("blue", "green", "red", "nir", "nirb", "re1", "re2", "re3", "swir1", "swir2"),
                         overwrite = FALSE){
  all_files_in_distribution <- list.files(path = envrmt$path_rworkflow, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep("_crop.tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  if ("all" %in% vi){
    vi <- c("NDVI", "NDWI", "NDBI", "NDBSI", "MSAVI")
  }
  message(paste0("Number of Scenes to calculate Indices for: ", number_of_tiffs))

  for (n in 1:number_of_tiffs){
    name <- gsub(".{9}$", "", tiff_paths[n])
    rstack <- terra::rast(file.path(envrmt$path_rworkflow, tiff_paths[n]))

    if ("NDVI" %in% vi){
      NDVI <- try(
        (rstack[[bands[4]]] - rstack[[bands[3]]]) / (rstack[[bands[4]]] + rstack[[bands[3]]])
        )
      NDVI[NDVI < -1] <- -1
      NDVI[NDVI > 1] <- 1
      names(NDVI) <- "NDVI"
      try(terra::add(rstack) <- NDVI)
    }# end NDVI

    if ("NDWI" %in% vi){
      NDWI <- try(
        (rstack[[bands[2]]] - rstack[[bands[4]]]) / (rstack[[bands[2]]] + rstack[[bands[4]]])
        )
        NDWI[NDWI < -1] <- -1
        NDWI[NDWI > 1] <- 1
        names(NDWI) <- "NDWI"
        try(terra::add(rstack) <- NDWI)
    }# end NDWI

    if ("NDBI" %in% vi){
      NDBI <- try(
        (rstack[[bands[9]]] - rstack[[bands[4]]]) / (rstack[[bands[9]]] + rstack[[bands[4]]])
        )
        NDBI[NDBI < -1] <- -1
        NDBI[NDBI > 1] <- 1
        names(NDBI) <- "NDBI"
        try(terra::add(rstack) <- NDBI)
    }# end NDBI

    if ("NDBSI" %in% vi){
      NDBSI <- try(
        ((rstack[[bands[3]]] + rstack[[bands[9]]]) - (rstack[[bands[4]]] + rstack[[bands[1]]])) /
          ((rstack[[bands[3]]] + rstack[[bands[9]]]) + (rstack[[bands[4]]] + rstack[[bands[1]]]))
        )
        NDBSI[NDBSI < -1] <- -1
        NDBSI[NDBSI > 1] <- 1
        names(NDBSI) <- "NDBSI"
        try(terra::add(rstack) <- NDBSI)
    }# end NDBSI

    if ("MSAVI" %in% vi){
      MSAVI <- try(
        rstack[[bands[4]]] +
          ((1 -
              sqrt((2 * rstack[[bands[4]]] + 1)^2 -
                    8 * (rstack[[bands[4]]] - rstack[[bands[3]]])))
          / 2)
        )
        MSAVI[MSAVI < -1] <- -1
        MSAVI[MSAVI > 1] <- 1
        names(MSAVI) <- "MSAVI"
        try(terra::add(rstack) <- MSAVI)
    }# end MSAVI

    message(paste0("Saving rasterstack with indices to rfinal [", envrmt$path_rfinal, "]."))
    terra::writeRaster(rstack,
                       file.path(envrmt$path_rfinal, paste0(name, "_ind.tif")),
                       overwrite = overwrite)

    gc()
  } # end n-loop
} # end function
