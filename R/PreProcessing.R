#' Cropping tiff data
#'
#' Crops input data to the extent size
#'
#' @param folder_path charackter. Path to input folder. All necessary and relevant data should
#' be stored here. Default: Input created by [climodr::hubs].
#' @param crs Coordinate reference system Used to crop all images in folder_path. If crs
#' it will automatically reprojected into this one. Default: crs of smallest Extent.
#' @param ext SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#' @param method character.
#' @param safe_output logical. If cropped data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
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
                     safe_output = FALSE,
                     ...) {
  tiff_list <- list();

  all_files_in_distribution <- list.files(path = Input, recursive = T); #reads all data in Input-Folder
  print(all_files_in_distribution);

  tiff_paths <- grep(".tif$", all_files_in_distribution, value=TRUE); # Select tiff-files
  number_of_tiffs <- length(tiff_paths);

  for (i in 1:number_of_tiffs){
    tiff_list[[i]] <- terra::rast(paste0(Input, tiff_paths[[i]]))
    if (is.null(ext)){
      tiff_list[[i]] <- terra::crop(tiff_list[[i]], terra::ext(terra::rast(paste0(Input, "res_area.tif"))))
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

  #for (i in 1:number_of_csvs){
  #  csv_list[[i]] <- read.csv(csv_paths[[i]])
  #};

  if (safe_output == TRUE){
    terra::writeRaster(tiff_stack, paste0(Output, "tiff_stack.tif"), overwrite = TRUE)
  #  write.csv(csv_list, paste0(alt_env_root_folder, "csv_list"), overwrite = TRUE)
  }
}

#' Cleaning CSV-Data
#'
#' Crops input data to the extent size
#'
#' @param method charackter. "proc" for ready-to-use data in seperate .csv-files. "tube" for raw-data in the right format. "AI" for raw-data in other formats. Default "proc"-Method.
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return List
#' @seealso
#'
#' @name check.csv
#' @export check.csv
#'
#' @examples
#'
check.csv <- function(method = "proc",
                      safe_output = FALSE,
                      ...){
  csv_list <- list();

  all_files_in_distribution <- list.files(path = Input, recursive = T); #reads all data in Input-Folder

  csv_paths <- grep(".csv$", all_files_in_distribution, value=TRUE);
  number_of_csvs <- length(csv_paths);

  for (i in 1:number_of_csvs){
    data <- read.csv(paste0(Input, csv_paths[[i]]))
    cn_data <- colnames(data)
    number_of_cn <- length(cn_data)

    for (j in 1:number_of_cn){
      sum(is.na(data$cn_data[j]))

    }
  };
}
