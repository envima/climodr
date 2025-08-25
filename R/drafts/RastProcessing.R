#' Prepare Raster Data
#'
#' Crops input data to the extent size and reprojects them into project Coordinate reference system.
#'
#' @param envrmt variable name of your envrmt list created using climodr's [envi.create] function. Default = envrmt.
#' @param datepos numeric vector. At which position in the filename does the date of your raster data start and where does it end?
#' @param dateformat cahracter. The format of your date. See [base::strptime()]
#' @param crs Coordinate reference system Used to crop all images in folder_path.
#' @param ext SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#' @param mask SpatVector or filename. Assign a polygon to mask your raster files. Either add file from global environment or the filename of your mask in Input/vector. Potentially reduces calculation times, as fewer pixels are contained in rasters after masking.
#' @param ...    arguments passed down from other functions.
#'
#' @return SpatRaster-Stack. Also saved to /workflow/rworkflow
#' @seealso `fin.csv`, `calc.indices`
#'
#' @name prepRasterData
#' @export prepRasterData
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#'
#'

prepRasterData <- function(envrmt = .GlobalEnv$envrmt,
                           datepos,
                           dateformat,
                           crs = NULL,
                           ext = NULL,
                           mask = NULL,
                           add_spectral_indices = FALSE,
                           bands = NULL,
                           save_output = "Bundle",
                           filename = NULL){
  # first, list contents of raster folder and filter for viable raster formarts
  raster_files <- list.files(envrmt$path_raster)

  formats <- sub(".*[.]", "", raster_files)
  raster_files <- raster_files[which(formats %in% c("tif", "gpkg", "shp", "vrt"))]
  formats <- formats[which(formats %in% c("tif", "gpkg", "shp", "vrt"))]

  # warn the user if scanning finds more then one file type.
  if(length(unique(formats)) > 1){
    warning("More then one filetype detected while scanning for raster data. Raster files of different types might be arranged differently and cause errors.")
  }

  # extract dates from filenames
  dates_o <- stringr::str_sub(raster_files,
                              start = datepos[1],
                              end = datepos[2])
  dates <- lubridate::fast_strptime(dates_o, tz = Sys.timezone(location = TRUE), format = dateformat)
  dates_o <- dates_o[which(!is.na(dates))] # so there is a vector with the og date names

  # Now read rasters for each date, first check cropping and projection values and mask
  res_area <- file.path(envrmt$path_dep, "res_area.tif")

  if(is.null(crs) & file.exists(res_area)){
    crs <- terra::crs(terra::rast(res_area))
  } else {
    stop("No valid coordinate reference system assigned in function arguments.\n Also could not find res_area.tif in Input/dep to extract one from.\n Please add valid coordinate reference system to arguments (see `?terra::crs` to find compatible ones) or make sure to have valid file with name `res_area.tif` in your Input/dep-Folder.\nStopping")
  }

  if(is.null(ext) & file.exists(res_area)){
    ext <- terra::ext(terra::rast(res_area))
  } else {
    stop("No valid extent assigned in function arguments.\n Also could not find res_area.tif in Input/dep to extract one from.\n Please add valid extent to arguments (see `?terra::ext` to find compatible ones) or make sure to have valid file with name `res_area.tif` in your Input/dep-Folder.\nStopping.")
  }

  if(!is.null(mask) & is.character(mask)){
    mask <- terra::vect(file.path(envrmt$path_vector, mask))
  }
  if(!is.null(mask)){
    mask <- terra::project(mask, crs)
  }


  # First, read rasters without date
  general_rasters <- raster_files[which(is.na(dates))]
  for(i in general_rasters){
    gr <- terra::rast(file.path(envrmt$path_raster, i))
    gr <- terra::project(gr, crs)
    ifelse(is.null(mask),
           gr <- terra::crop(gr, ext),
           gr <- terra::mask(gr, mask))
    ifelse(i == general_rasters[1],
           grs <- gr,
           grs <- c(grs, gr))
    remove(gr)
  }
  terra::writeRaster(grs, file.path(envrmt$path_rfinal, "undated_rasters.tif"))

  # Then, read rasters with date
  dated_rasters <- raster_files[which(!is.na(dates))]
  for (i in 1:length(unique(dates_o))){
    read_rasters <- paste(envrmt$path_raster, dated_rasters[which(unique(dates_o)[i] %in% stringr::str_sub(dated_rasters, datepos[1], datepos[2]))], sep = "/")
    for (j in read_rasters){
      dr <- terra::project(terra::rast(j), crs)
      ifelse(is.null(mask),
             dr <- terra::crop(dr, ext),
             dr <- terra::mask(dr, mask))
      ifelse(j == read_rasters[i],
             drs <- dr,
             drs <- c(drs, dr))
      remove(dr)
    } # end reading dated rasters
    if(add_spectral_indices & !is.null(bands)){
      drs <- addSpectralIndices(x = drs,
                                bands = bands,
                                indices = "all",
                                add_to_x = TRUE,
                                stop_process = FALSE,
                                outliers = "cap")
    }
    terra::time(drs) <- rep(dates[which(!is.na(dates))][i], terra::nlyr(drs))

    # save files according to input argument
    if(output == "single"){
      outname <- paste(unique(dates_o)[i], dateformat, ("single_raster.tif"), sep = "__")
      terra::writeRaster(drs, file.path(envrmt$path_rworkflow, outname), overwrite = TRUE)
      ifelse(i == 1,
             outfile <- outname,
             outfile[i] <- outname)
    }
    if(output == "bundle"){
      ifelse(i == 1,
             outfile <- drs,
             terra::add(outfile) <- drs)
    }
  } # end i loop

  if(output == "bundle"){
    terra::writeRaster(outfile,
                       file.path(envrmt$path_rfinal, "bundled_rasters.tif"),
                       overwrite = TRUE)
  }
  return(outfile)
} # end function





# Add Indices to function
addSpectralIndices <- function(x,
                               bands = c("blue", "green", "red", "nir", "swir"),
                               indices = "all",
                               add_to_x = TRUE,
                               stop_process = TRUE,
                               outliers = NULL){

  # quick function for adding more indexes easy, not for export
  calc.index <- function(name, formula, min = -1, max = 1){
    if (indices == "all" | name %in% indices){
      Index <- formula
      if(! all(min < terra::values(Index) & terra::values(Index) < max, na.rm = TRUE)){
        message(paste0("Some cell values for the ", name, " lie outside of the expected range."))
        if (outliers == "cap"){
          NDVI[NDVI < min] <- min
          NDVI[NDVI > max] <- max
        }
        if (outliers == "remove"){
          NDVI[NDVI < min] <- NA
          NDVI[NDVI > max] <- NA
        }
      }
      names(Index) <- name
      return(Index)
    }
  }# end calc.index

  data_ind <- terra::rast()

  if(!all(bands %in% names(x))){
    message <- paste0("Not all bands mentioned in Input match bands available in x. Check your Inputs.\n Available bands: ", paste(names(x), collapse = " "),"\n User Input: ", paste(bands, collapse = " "),"\nCan't calculate Indices.")
    if(stop_process){
      stop(paste0(message, "\nStopping."))
    } else {
      warning(message)
    }
  } else {
    # NDVI
    terra::add(data_ind) <- calc.index(
      "NDVI",
      formula = (x[[bands[4]]] - x[[bands[3]]]) / (x[[bands[4]]] + x[[bands[3]]])
      )
    # NDWI
    terra::add(data_ind) <- calc.index(
      "NDWI",
      formula = (x[[bands[2]]] - x[[bands[4]]]) / (x[[bands[2]]] + x[[bands[4]]])
      )
    # NDBI
    terra::add(data_ind) <- calc.index(
      "NDBI",
      formula = (x[[bands[5]]] - x[[bands[4]]]) / (x[[bands[5]]] + x[[bands[4]]])
      )
    # NDBSI
    terra::add(data_ind) <- calc.index(
      "NDBSI",
      formula = ((x[[bands[3]]] + x[[bands[5]]]) - (x[[bands[4]]] + x[[bands[1]]])) / ((x[[bands[3]]] + x[[bands[5]]]) + (x[[bands[4]]] + x[[bands[1]]]))
    )
    # MSAVI
    terra::add(data_ind) <- calc.index(
      "MSAVI",
      formula = x[[bands[4]]] + ((1 - sqrt((2 * x[[bands[4]]] + 1)^2 - 8 * (x[[bands[4]]] - x[[bands[3]]]))) / 2)
    )
  }

  if(add_to_x){
    data_ind <- c(x, data_ind)
  }

  return(data_ind)
}
