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
                     ...) {
  all_files_in_distribution <- list.files(path = envrmt$path_raster, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep(".tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    print(paste0("Reading in raster ", i, "/", length(number_of_tiffs), "."))
    tif <- terra::rast(file.path(envrmt$path_raster, tiff_paths[[i]]))

    print("Cropping raster...")
    if (is.null(ext)){
      tif <- terra::crop(tif, terra::rast(file.path(envrmt$path_dep, "res_area.tif")), mask = TRUE)
    } else {
      tif <- terra::crop(tif, ext, mask = TRUE)
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
      print("Saving raster to rworkflow.")
      terra::writeRaster(tif, file.path(envrmt$path_rworkflow,
                                        paste0(stringr::str_sub(tiff_paths[i], 1, 12),
                                               "_crop.tif")
                                        )
                         )
    } # end MB_timeseries_loop
  };

  if (method == "Singleband"){
    return(tiff_stack);
    print("Saving rasterstack to rworkflow.")
    terra::writeRaster(tiff_stack, file.path(envrmt$path_rworkflow, "tiff_stack.tif"), overwrite = TRUE)

  }; # end 2nd Singleband Loop
}

#' Calculate spectral indices
#'
#' Calculates a set of spectral indices
#'
#' @param method charackter.
#' @param crs Coordinate reference system Used to crop all images in folder_path. If crs
#' it will automatically reprojected into this one. Default: crs of smallest Extent.
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

calc.indices <- function(vi = NULL){
  all_files_in_distribution <- list.files(path = envrmt$path_rworkflow, recursive = T); #reads all data in Raster Input Folder
  tiff_paths <- grep("_crop.tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    name <- stringr::str_sub(tiff_paths[i], 1, 12)
    print(paste0("Calculate indices for ", name, "...  ", i, "/", number_of_tiffs))
    data <- terra::rast(file.path(envrmt$path_rworkflow, tiff_paths[i]))
    indices <- RStoolbox::spectralIndices(data,
                                          blue = "blue",
                                          green = "green",
                                          red = "red",
                                          nir = "nir",
                                          redEdge1 = "re1",
                                          redEdge2 = "re2",
                                          redEdge3 = "re3",
                                          swir2 = "swir1",
                                          swir3 = "swir2",
                                          indices = vi,
                                          scaleFactor = 1,
                                          skipRefCheck = TRUE)
    data_vi <- terra::rast(indices)
    data_fi <- c(data, data_vi)

    print("Writing Raster...")
    terra::writeRaster(data_fi, file.path(envrmt$path_rfinal, paste0(name, "_ind.tif")))
    remove(data, indices, data_vi, data_fi)
    gc()
  }
}
