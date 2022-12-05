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

#' Preparing CSV-Data
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
#' @name prep.csv
#' @export prep.csv
#'
#' @examples
#'
prep.csv <- function(method = "proc",
                     safe_output = TRUE,
                     ...){
  csv_list <- list();
  Input <- envrmt$path_tabular

  all_files_in_distribution <- list.files(path = Input, recursive = T); #reads all data in Input-Folder

  csv_paths <- grep(".csv$", all_files_in_distribution, value=TRUE);
  number_of_csvs <- length(csv_paths);

  for (i in 1:number_of_csvs){
    csv_data <- read.csv(file.path(Input, paste0(csv_paths[[i]])), sep = ",")
    cn_data <- colnames(csv_data)
    number_of_cn <- length(cn_data)

    csv_data$daymonth <- strftime(csv_data[,2], format = "%m-%d")
    csv_data$year<- strftime(csv_data[,2], format = "%y")
    csv_data$month <- strftime(csv_data[,2], format = "%m")
    csv_data$day <- strftime(csv_data[,2], format = "%d")

    csv_data <- csv_data[, c(cn_data[1:2], "daymonth", "year", "month", "day", cn_data[3:number_of_cn])]

    cn_data <- colnames(csv_data)
    number_of_cn <- length(cn_data)

    for (j in 7:number_of_cn){
      csv_data <- transform(csv_data, temp_col = ave(csv_data[,j], daymonth, FUN = function(x) mean(x, na.rm = TRUE)))
      csv_data[,j] <- ifelse(is.na(csv_data[,j]), csv_data$temp_col, csv_data[,j])
      csv_data$temp_col <- NULL
    }

    csv_data$daymonth <- NULL

    write.csv(csv_data, file.path(envrmt$path_tworkflow, paste0(csv_data[[1,1]], "_no_NAs.csv")), row.names = FALSE)
  };
}

#' Processing CSV-Data
#'
#' Calculate necessary Data from Stationary Data
#'
#' @param method character.
#' @param rbind logical.
#' @param safe_output logical. If data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return List
#' @seealso
#'
#' @name proc.csv
#' @export proc.csv
#'
#' @examples
#'
proc.csv <- function(method = "all",
                     rbind = TRUE,
                     safe_output = TRUE,
                     ...){
  csv_list <- list();

  all_files_in_distribution <- list.files(path = envrmt$path_tworkflow, recursive = T); #reads all data in Input-Folder

  csv_paths <- grep("_no_NAs.csv$", all_files_in_distribution, value=TRUE);
  number_of_csvs <- length(csv_paths);

  for (i in 1:number_of_csvs){

    x <- read.csv(file.path(envrmt$path_tworkflow, csv_paths[i]))

    cn_x <- colnames(x)
    number_of_cn <- length(cn_x)

    for (j in 6:number_of_cn){
      fo <- as.formula(paste(cn_x[j], " ~ month + year"))
      y <- aggregate(fo, x, mean)
      colnames(y) <- c("month","year",paste0("monthly_mean_", cn_x[j]))

      y$plot <- x[[1,1]]

      y <- y[,c("plot","year","month",paste0("monthly_mean_", cn_x[j]))]

      if (j == 6){
        mm <- y
      } else {
        mm <- cbind(mm, y[4])
      }
    }
    if (rbind == FALSE){
      write.csv(mm, file.path(envrmt$path_tworkflow, paste0("csv_", i, "_mm.csv")), row.names = FALSE)
    } else {
      if (i == 1) {
        mm_t <- mm
      } else {
        mm_t <- rbind(mm_t, mm)
      }
    }
  }
  if (rbind == TRUE){
    if (safe_output == TRUE){
      write.csv(mm_t, file.path(envrmt$path_tworkflow, "all_monthly_means.csv"), row.names = FALSE)
    }
    return(mm_t)
  }
}

#' Spatial agregation for CSV-Data
#'
#' blabla
#'
#' @param
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return List
#' @seealso
#'
#' @name spat.csv
#' @export spat.csv
#'
#' @examples
#'
spat.csv <- function(method = "monthly",
                     des_file,
                     safe_output = TRUE,
                     ...){
  if (method == "monthly"){
    data <- read.csv(file.path(envrmt$path_tworkflow, "all_monthly_means.csv"));
    cn_data <- colnames(data)
    names_of_stations <- as.vector(unlist(unique(data[1])))
    number_of_stations <- length(names_of_stations)
  }
  if (method == "daily"){
    csv_list <- list()
    all_files_in_distribution <- list.files(path = envrmt$path_tworkflow, recursive = T)
    csv_paths <- grep("_no_NAs.csv$", all_files_in_distribution, value=TRUE)
    number_of_csvs <- length(csv_paths)

    for (i in 1:number_of_csvs){

      x <- read.csv(file.path(envrmt$path_tworkflow, paste0(csv_paths[i])))
      x <- data.frame(x)

      if (i == 1) {
        data <- x
      } else {
        data <- rbind(data, x)
      }
    }
    cn_data <- colnames(data)
    names_of_stations <- as.vector(unlist(unique(data[1])))
    number_of_stations <- length(names_of_stations)
  };

  des <- read.csv(file.path(envrmt$path_dep, des_file));

  data$lat <- "";
  data$lon <- "";
  data$elevation <- "";

  for (i in 1:number_of_stations){
    data$lat[which(data$plot == names_of_stations[i])] <- des$lat[which(grepl(names_of_stations[i], des$plot))]
    data$lon[which(data$plot == names_of_stations[i])] <- des$lon[which(grepl(names_of_stations[i], des$plot))]
    data$elevation[which(data$plot == names_of_stations[i])] <- des$elevation[which(grepl(names_of_stations[i], des$plot))]
  };

  if (method == "monthly"){
    data$day <- 1
    data$datetime <-as.Date(with(data,paste(year,month,day,sep="-")),"%y-%m-%d");
    data <- data[,c(cn_data[1],
                    "datetime",
                    cn_data[2:3],
                    "day",
                    cn_data[4:length(cn_data)],
                    "lat",
                    "lon",
                    "elevation"
    )];

    mms <- 6:(2 + length(cn_data));

    for (i in mms){
      data[i] <- round(data[i], digits = 3)
    };

    names(data)  <- c(names(data[1:5]),
                      gsub("monthly_mean_", "", names(data[mms])),
                      names(data[(3 + length(cn_data)):length(names(data))])
    );

    write.csv(data, file.path(envrmt$path_tworkflow, "spat_monthly_means.csv"), row.names = FALSE);
    return(data)
  };

  if (method == "daily") {
    for (i in 6:length(cn_data)){
      data[i] <- round(data[i], digits = 3)
    };

    write.csv(data, file.path(envrmt$path_tworkflow, "spat_daily.csv"), row.names = FALSE);
    return(data)
  }
}

#' Final agregation for CSV-Data
#'
#' blabla
#'
#' @param
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return List
#' @seealso
#'
#' @name final.csv
#' @export final.csv
#'
#' @examples
#'
