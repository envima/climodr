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
crop.all <- function(method = "Input",
                     crs = NULL,
                     ext = NULL,
                     ...) {
  tiff_list <- list();
  Input <- envrmt$path_raster

  all_files_in_distribution <- list.files(path = Input, recursive = T); #reads all data in Raster Input Folder

  tiff_paths <- grep(".tif$", all_files_in_distribution, value=TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    tiff_list[[i]] <- terra::rast(file.path(Input, tiff_paths[[i]]))

    if (is.null(ext)){
      tiff_list[[i]] <- terra::crop(tiff_list[[i]], terra::ext(terra::rast(file.path(envrmt$path_dep, "res_area.tif"))))
    } else {
      tiff_list[[i]] <- terra::crop(tiff_list[[i]], ext)
    }

    if (i == 1){
      tiff_stack <- tiff_list[[i]]
    }
    else {
      terra::add(tiff_stack) <- tiff_list[[i]]
    }
  };

  if (!is.null(crs)){
    if (is.null(crs(tiff_stack[[i]]))){
      crs(tiff_list[[i]] <- crs)
    }
    else {
      if (!crs(tiff_stack[[i]]) == crs){
        tiff_stack[[i]] <- terra::project(tiff_stack[[i]], crs)
      }
    }
  };
  return(tiff_stack);
  terra::writeRaster(tiff_stack, file.path(envrmt$path_rworkflow, "tiff_stack.tif"), overwrite = TRUE)
}
