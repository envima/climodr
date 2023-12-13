#' Preparing CSV-Data
#'
#' Crops input data to the extent size
#'
#' @param method character. "proc" for ready-to-use data in seperate .csv-files. "tube" for raw-data in the right format. "AI" for raw-data in other formats. Default "proc"-Method.
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

# Checkup for empty columns in the data, which creates a warning.

  csv_list <- list();
  Input <- envrmt$path_tabular

  all_files_in_distribution <- list.files(path = Input, recursive = T); #reads all data in Input-Folder

  csv_paths <- grep(".csv$", all_files_in_distribution, value = TRUE);
  number_of_csvs <- length(csv_paths);

  for (i in 1:number_of_csvs){
    csv_data <- read.csv(file.path(Input, paste0(csv_paths[[i]])), sep = ",")
    cn_data <- colnames(csv_data)
    number_of_cn <- length(cn_data)

    csv_data$daymonth <- strftime(csv_data[,2], format = "%m-%d")
    csv_data$year <- strftime(csv_data[,2], format = "%Y")
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
    csv_data <- tidyr::drop_na(csv_data)
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
      fo <- as.formula(paste(cn_x[j], " ~ day + month + year"))
      y <- aggregate(fo, x, mean)
      colnames(y) <- c("day", "month", "year", paste0("daily_mean_", cn_x[j]))

      y$plot <- x[[1,1]]

      y <- y[,c("plot", "year", "month", "day", paste0("daily_mean_", cn_x[j]))]

      if (j == 6){
        dm <- y
      } else {
        dm <- cbind(dm, y[5])
      }
    }
    if (rbind == FALSE){
      write.csv(dm, file.path(envrmt$path_tworkflow, paste0("csv_", i, "_dm.csv")), row.names = FALSE)
    } else {
      if (i == 1) {
        dm_t <- dm
      } else {
        dm_t <- rbind(dm_t, dm)
      }
    }
  }
  if (rbind == TRUE){
    if (safe_output == TRUE){
      write.csv(dm_t, file.path(envrmt$path_tworkflow, "all_daily_means.csv"), row.names = FALSE)
    }
    return(dm_t)
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
                     crs = NULL,
                     safe_output = TRUE,
                     ...){
  if (method == "monthly"){
    data <- read.csv(file.path(envrmt$path_tworkflow, "all_daily_means.csv"));
    cn_data <- colnames(data)
    names_of_stations <- as.vector(unlist(unique(data[1])))
    number_of_stations <- length(names_of_stations)
  }; # end monthly loop 1

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
  }; # end daily loop 1

  des <- read.csv(file.path(envrmt$path_dep, des_file));

  data$lat <- "";
  data$lon <- "";

  for (i in 1:number_of_stations){
    data$lat[which(data$plot == names_of_stations[i])] <- des$lat[which(grepl(names_of_stations[i], des$plot))]
    data$lon[which(data$plot == names_of_stations[i])] <- des$lon[which(grepl(names_of_stations[i], des$plot))]
  }; # end coordinate loop

  if (method == "monthly"){
    daily_means <- 5:length(cn_data);

    for (i in daily_means){
      data[i] <- round(data[i], digits = 3)
    };

    names(data)  <- c(names(data[1:4]),
                      gsub("daily_mean_", "", names(data[daily_means])),
                      names(data[(5 + length(daily_means)):length(names(data))])
    );

    if (is.null(crs)){
      crs <- terra::crs(terra::rast(file.path(envrmt$path_dep, "res_area.tif")))
    }

    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)
    sp::coordinates(data) <- ~ lon + lat
    sp::proj4string(data) <- sp::CRS("+proj=longlat +datum=WGS84")
    data <- sp::spTransform(data, crs)

    n <- names(data)
    data <- data.frame(data)
    data$optional <- NULL
    data <- data[, c(n, "lat", "lon")]

    write.csv(data, file.path(envrmt$path_tworkflow, "spat_daily_means.csv"), row.names = FALSE);
    return(data)
  };

  if (method == "daily") {
    for (i in 6:length(cn_data)){
      data[i] <- round(data[i], digits = 3)
    };

    if (is.null(crs)){
      crs <- terra::crs(terra::rast(file.path(envrmt$path_dep, "res_area.tif")))
    }

    sp::coordinates(data) <- ~ lon + lat
    sp::proj4string(data) <- sp::CRS("+proj=longlat +datum=WGS84")
    data <- sp::spTransform(data, crs)

    n <- names(data)
    data <- data.frame(data)
    data$optional <- NULL
    data <- data[, c(n, "lat", "lon")]

    write.csv(data, file.path(envrmt$path_tworkflow, "spat_hourly_means.csv"), row.names = FALSE);
    return(data)
  }
}

#' Final aggregation for CSV-Data
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
fin.csv <- function(method = "monthly",
                    crs = NULL,
                    safe_output = TRUE,
                    ...){
  if (method == "monthly"){
    data_o <- read.csv(file.path(envrmt$path_tworkflow, "spat_daily_means.csv"));
    data_o$ID <- seq(1:nrow(data_o))
  };

  all_files_in_distribution <- list.files(path = file.path(envrmt$path_rfinal), recursive = T); #reads all data in Workflow Raster Folder

  sat_paths <- grep("mean.tif$", all_files_in_distribution, value = TRUE); # Select tiff-files
  dgm_path <- grep("dgm.tif$", all_files_in_distribution, value = TRUE);

  if (method == "monthly"){
    dgm <- terra::rast(file.path(envrmt$path_rfinal, dgm_path))
    extr_total <- data.frame()

    for (i in 1:length(sat_paths)){
      month <- as.numeric(stringr::str_sub(sat_paths[i], 9, 10))
      tiff <- terra::rast(file.path(envrmt$path_rfinal, sat_paths[i]))
      data_sub <- data_o[which(data_o$month == month),]

      extr <- terra::extract(tiff,
                             data.frame(x = data_sub$lon,
                                        y = data_sub$lat)
                            );
      extr$ID <- data_sub$ID
      extr_total <- rbind(extr_total, extr)
    } # end monthly sat_paths loop

    extr_dgm <- terra::extract(dgm,
                               data.frame(x = data_o$lon,
                                          y = data_o$lat)
                               )
    extr <- merge(extr_dgm, extr_total, by = "ID")
    data <- merge(data_o, extr, by = "ID")
    data$ID <- NULL;
    data$EVI <- NULL;
    data$EVI2 <- NULL;
    data_n <- tidyr::drop_na(data)

    write.csv(data_n, file.path(envrmt$path_tfinal, "final_daily.csv"), row.names = FALSE);
    return(data)
  } # end monthly loop

  if (method == "anual"){
    tiff_list <- list()
    for (i in 1:length(sat_paths)){
      tiff_list[[i]] <- terra::rast(file.path(envrmt$path_rworkflow, tiff_paths[[i]]))
      if (i == 1){
        tiff_stack <- tiff_list[[i]]
      } else {
        terra::add(tiff_stack) <- tiff_list[[i]]
      }
    }

    data$ID <- seq(1:length(data[,1]));
    extr <- terra::extract(tiff_stack,
                           data.frame(x = data$lon,
                                      y = data$lat)
    );
    data <- merge(data, extr, by = "ID");
    data$ID <- NULL;
    data$datetime <- NULL;
    data_n <- tidyr::drop_na(data)

    write.csv(data_n, file.path(envrmt$path_tfinal, "final_anualy.csv"), row.names = FALSE);
    return(data)
  }; # end anual loop
}
