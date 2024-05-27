#' Cropping tiff data
#'
#' Crops input data to the extent size
#'
#' @param method charackter.
#' @param crs Coordinate reference system Used to crop all images in folder_path. If crs
#' it will automatically reprojected into this one. Default: crs of smallest Extent.
#' @param ext SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#'
#' @return SpatRaster-Stack
#' @seealso
#'
#' @name crop.all
#' @export crop.all
#'
#' @examples
#'
crop.all <- function(method = "MB_Timeseries",
                     crs = NULL,
                     ext = NULL,
                     overwrite = FALSE,
                     ...) {
  all_files_in_distribution <- list.files(path = envrmt$path_raster, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep(".tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    print(paste0("Reading in raster ", i, "/", number_of_tiffs, "."))
    tif <- terra::rast(file.path(envrmt$path_raster, tiff_paths[[i]]))

    print("Cropping raster...")
    if (is.null(ext)){
      tif <- terra::project(tif, terra::rast(file.path(envrmt$path_dep, "res_area.tif")), mask = TRUE)
    } else {
      tif <- terra::project(tif, ext, mask = TRUE)
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
          print("Adding coordinate reference system to raster...")
          terra::crs(tiff_stack[[i]]) <- crs
        }
        else {
          if (!terra::crs(tiff_stack[[i]]) == crs){
            print("Reprojecting raster...")
            tiff_stack[[i]] <- terra::project(tiff_stack[[i]], crs)
          }
        }
      } # end crs loop
    } # end singleband loop

    if (method == "MB_Timeseries"){
      if (!is.null(crs)){
        if (is.null(terra::crs(tif))){
          print("Adding coordinate reference system to raster...")
          terra::crs(tif) <- crs
        } else {
          if (!terra::crs(tif) == crs){
            print("Reprojecting raster...")
            tif <- terra::project(tif, crs)
          }}
        } # end crs loop

      if(grepl("dgm", gsub(".{4}$", "", tiff_paths[i]), fixed = TRUE)){
        print("Saving DGM to rfinal.")
        terra::writeRaster(tif, file.path(envrmt$path_rfinal,
                                          paste0(gsub(".{4}$", "", tiff_paths[i]),
                                                 "_crop.tif")),
                           overwrite = overwrite)
      } else {
        print("Saving raster to rworkflow.")
        terra::writeRaster(tif, file.path(envrmt$path_rworkflow,
                                          paste0(gsub(".{4}$", "", tiff_paths[i]),
                                                 "_crop.tif")),
                           overwrite = overwrite)
      }

      print(tif)
    } # end MB_timeseries_loop
  };

  if (method == "Singleband"){
    return(tiff_stack);
    print("Saving rasterstack to rworkflow.")
    terra::writeRaster(tiff_stack, file.path(envrmt$path_rworkflow, "tiff_stack.tif"), overwrite = overwrite)

  }; # end 2nd Singleband Loop
}

#' Calculate spectral indices
#'
#' Calculates a set of spectral indices
#'
#' @param method character.
#' @param ext SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#'
#' @return SpatRaster-Stack
#' @seealso
#'
#' @name calc.indices
#' @export calc.indices
#'
#' @examples
#'

calc.indices <- function(vi = "all",
                         bands = c("blue", "green", "red", "nir", "nirb", "re1", "re2", "re3", "swir1", "swir2"),
                         overwrite = FALSE){
  all_files_in_distribution <- list.files(path = envrmt$path_rworkflow, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep("_crop.tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  print(paste0("Number of Scenes to calculate Indices for: ", number_of_tiffs))

  for (n in 1:number_of_tiffs){
    name <- gsub(".{9}$", "", tiff_paths[n])
    rstack <- terra::rast(file.path(envrmt$path_rworkflow, tiff_paths[n]))

    NDVI <- try(
      (rstack[[bands[4]]] - rstack[[bands[3]]]) / (rstack[[bands[4]]] + rstack[[bands[3]]])
      )
      NDVI[NDVI < -1] <- -1
      NDVI[NDVI > 1] <- 1
      names(NDVI) <- "NDVI"
      try(terra::add(rstack) <- NDVI)
    # end NDVI

    NDWI <- try(
      (rstack[[bands[2]]] - rstack[[bands[4]]]) / (rstack[[bands[2]]] + rstack[[bands[4]]])
      )
      NDWI[NDWI < -1] <- -1
      NDWI[NDWI > 1] <- 1
      names(NDWI) <- "NDWI"
      try(terra::add(rstack) <- NDWI)
    # end NDWI

    NDBI <- try(
      (rstack[[bands[9]]] - rstack[[bands[4]]]) / (rstack[[bands[9]]] + rstack[[bands[4]]])
      )
      NDBI[NDBI < -1] <- -1
      NDBI[NDBI > 1] <- 1
      names(NDBI) <- "NDBI"
      try(terra::add(rstack) <- NDBI)
    # end NDBI

    NDBSI <- try(
      ((rstack[[bands[3]]] + rstack[[bands[9]]]) - (rstack[[bands[4]]] + rstack[[bands[1]]])) /
        ((rstack[[bands[3]]] + rstack[[bands[9]]]) + (rstack[[bands[4]]] + rstack[[bands[1]]]))
      )
      NDBSI[NDBSI < -1] <- -1
      NDBSI[NDBSI > 1] <- 1
      names(NDBSI) <- "NDBSI"
      try(terra::add(rstack) <- NDBSI)
    # end NDBSI

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
    # end MSAVI

    EVI <- try(
      ((rstack[[bands[4]]] - rstack[[bands[3]]]) /
         (rstack[[bands[4]]] + 6 * rstack[[bands[3]]]) - 7.5 * rstack[[bands[1]]] + 1) * 2.5
      )
      EVI[EVI < -1] <- -1
      EVI[EVI > 1] <- 1
      names(EVI) <- "EVI"
      try(terra::add(rstack) <- EVI)
    # end EVI

    terra::writeRaster(rstack,
                       file.path(envrmt$path_rfinal, paste0(name, "_ind.tif")),
                       overwrite = overwrite)

    gc()
  } # end n-loop
} # end function

#' Aggregate Rasters
#'
#' Aggregate spatial raster files for desired timespan (eg. daily, monthly, annual..)
#'
#' @param method character.
#'
#' @return SpatRaster-Stack
#' @seealso
#'
#' @name aggregate.rast
#' @export aggregate.rast
#'
#' @examples
#'

aggregate.rast <- function(){
  all_files <- list.files(path = file.path(envrmt$path_rfinal), recursive = T); #reads all data in Workflow Raster Folder
  dates <- as.Date(stringr::str_sub(all_files, 5, 12), format = "%Y%m%d")
  months <- strftime(dates, format = "%m")

  for (i in unique(months)){
    print(paste0("Calculating means for ",
                 unique(strftime(dates, format = "%B"))[as.integer(i)],
                 "..  ",
                 i, "/", length(unique(months))))
    month_rasters <- all_files[which(months == i)]
    rstack <- terra::rast()
    rmean <- terra::rast()

    print(paste0("Reading ", length(month_rasters), " files for this month..  "))

    for (j in 1:length(month_rasters)){
      x <- terra::rast(file.path(envrmt$path_rfinal, month_rasters[j]))
      terra::add(rstack) <- x
    } # end j-loop

    n <- terra::nlyr(rstack)
    l <- length(terra::sources(rstack))
    vsort <- names(rstack)[1:(n/l)]
    rstack <- rstack[[sort(names(rstack))]]

    print("Calculate monthly means for each layer.. ")
    for (k in seq(1, n, l)){
      print("  ...  ")
      a <- rstack[[k:(k+l-1)]]
      b <- terra::app(a, fun = "mean", na.rm = TRUE)
      names(b) <- names(a)[1]
      terra::add(rmean) <- b
    } # end k-loop

    print("Writing raster..")
    terra::writeRaster(rmean,
                       file.path(envrmt$path_rfinal,
                                 paste0("hai_2020",
                                        unique(months)[as.integer(i)],
                                        "_mean.tif")
                       )
    )
    remove(rstack, rmean, j, n, l, vsort, k, a, b)
    gc()
  } # end i-loop

  data_order <- names(terra::rast(file.path(envrmt$path_rworkflow, "hai_20200104_ind.tif")))
  all_files <- list.files(path = file.path(envrmt$path_rfinal), recursive = T)

  for (i in 1:12){
    data <- terra::rast(file.path(envrmt$path_rfinal, all_files[i]))
    data <- data[[data_order]]
    data$EVI <- NULL
    data$EVI2 <- NULL
    print(i)
    terra::writeRaster(data, file.path(envrmt$path_rfinal, "new", all_files[i]))
  }
}
