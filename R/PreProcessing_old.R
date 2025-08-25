#_____________________________________________________________________________#
#_____________________________________________________________________________#
#' Preparing CSV-Data
#'
#' Crops input data to the extent size and removes NA-Values
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method character. "proc" for ready-to-use data in separate .csv-files. "tube" for raw-data from the Tube Data Base. Default "proc"-Method.
#' @param save_output logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE.
#' Otherwise the output will be saved in the temporary directory. Default: FALSE.
#' @param ...      arguments passed down from other functions.
#'
#' @return List
#' @seealso `proc.csv`, `spat.csv`, `fin.csv`
#'
#' @name prep.csv
#' @export prep.csv
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' #prepare csv-files
#' prep.csv(envrmt = envrmt,
#'          method = "proc",
#'          save_output = TRUE)
#'
#' #check the created csv files
#' csv_files <- grep("_no_NAs.csv$",
#'                   list.files(envrmt$path_tworkflow),
#'                   value=TRUE)
#' csv_files
#'
prep.csv <- function(envrmt = .GlobalEnv$envrmt,
                     method = "proc",
                     save_output = TRUE,
                     ...){

# Checkup for empty columns in the data, which creates a warning.
# ---> Still not done

# --------------------------------------------------------------------------- #
# Method == "proc"
  if (method == "proc"){
    csv_list <- list()
    all_files_in_distribution <- list.files(
      path = envrmt$path_tabular,
      recursive = T
    ) #reads all data in Input-Folder
    csv_paths <- grep(
      ".csv$",
      all_files_in_distribution,
      value = TRUE
    )
    number_of_csvs <- length(csv_paths)

    for (i in 1:number_of_csvs){
      csv_data <- utils::read.csv(
        file.path(
          envrmt$path_tabular,
          paste0(csv_paths[[i]])),
        sep = ","
      )
      cn_data <- colnames(csv_data)
      number_of_cn <- length(cn_data)

      csv_data$daymonth <- strftime(csv_data[,2], format = "%m-%d")
      csv_data$year <- strftime(csv_data[,2], format = "%Y")
      csv_data$month <- strftime(csv_data[,2], format = "%m")
      csv_data$day <- strftime(csv_data[,2], format = "%d")

      csv_data <- csv_data[,
                           c(cn_data[1:2],
                             "daymonth",
                             "year",
                             "month",
                             "day",
                             cn_data[3:number_of_cn]
                           )
      ]

      cn_data <- colnames(csv_data)
      number_of_cn <- length(cn_data)

      for (j in 7:number_of_cn){
        csv_data <- transform(
          csv_data,
          temp_col = stats::ave(
            csv_data[, j],
            "daymonth",
            FUN = function(x) mean(x, na.rm = TRUE)
          )
        )
        csv_data[, j] <- ifelse(
          is.na(csv_data[, j]),
          csv_data$temp_col,
          csv_data[, j]
        )
        csv_data$temp_col <- NULL
      }

      csv_data <- tidyr::drop_na(csv_data)
      csv_data$daymonth <- NULL

      utils::write.csv(
        csv_data,
        file.path(
          envrmt$path_tworkflow,
          paste0(csv_data[[1, 1]],
                 "_no_NAs.csv")
        ),
        row.names = FALSE
      )
    }
    message(
      paste0(
        "Removed NAs from ",
        number_of_csvs,
        " stations"
      )
    )
  } # end proc - method
# --------------------------------------------------------------------------- #
} # end function


#_____________________________________________________________________________#
#_____________________________________________________________________________#
#' Processing CSV-Data
#'
#' Calculate averaged sensor values aggregated to a given time interval.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method character. Either "daily", monthly" or "annual". Also depends on the available data.
#' @param rbind logical. Create a single file with all climate stations. If FALSE, every station will be saved in a seperate file.
#' @param save_output logical. If data should be saved permanently in the Environment put save_output = TRUE.
#' Otherwise the output will be saved in the temporary directory. Default: TRUE.
#' @param ...      arguments passed down from other functions.
#'
#' @return List
#' @seealso `prep.csv`, `spat.csv`, `fin.csv`
#'
#' @name proc.csv
#' @export proc.csv
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' #prepare csv-files
#' prep.csv(envrmt = envrmt,
#'          method = "proc",
#'          save_output = TRUE)
#'
#' #process csv-files
#' csv_data <- proc.csv(envrmt = envrmt,
#'                      method = "monthly",
#'                      rbind = TRUE,
#'                      save_output = TRUE)
#' head(csv_data)
#'
proc.csv <- function(envrmt = .GlobalEnv$envrmt,
                     method = "monthly",
                     rbind = TRUE,
                     save_output = TRUE,
                     ...){
# Method == "monthly"
  if (method == "monthly"){
    csv_list <- list()
    all_files_in_distribution <- list.files(
      path = envrmt$path_tworkflow,
      recursive = T
    ) # reads all data in Input-Folder

    csv_paths <- grep(
      "_no_NAs.csv$",
      all_files_in_distribution,
      value = TRUE
    )
    number_of_csvs <- length(csv_paths)

    for (i in 1:number_of_csvs){
      x <- utils::read.csv(
        file.path(
          envrmt$path_tworkflow,
          csv_paths[i]
        )
      ) # read in one csv file per i

      cn_x <- colnames(x)
      number_of_cn <- length(cn_x)

      for (j in 6:number_of_cn){
        fo <- stats::as.formula(
          paste(cn_x[j], " ~ month + year")
        )
        y <- stats::aggregate(fo, x, mean)
        colnames(y) <- c(
          "month",
          "year",
          paste0(
            "monthly_mean_",
            cn_x[j]
          )
        )
        y$plot <- x[[1, 1]]
# Add datetime for spacetimefolds later
        y$datetime <- with(y, ISOdate(year, month, 1))

        y <- y[,
               c("plot",
                 "datetime",
                 "year",
                 "month",
                 paste0(
                   "monthly_mean_",
                   cn_x[j]
                 )
               )
            ]

        ifelse(j == 6,
               dm <- y,
               dm <- cbind(dm, y[5])
               )
      } # end j loop

      if (rbind == FALSE){
        utils::write.csv(
          dm,
          file.path(
            envrmt$path_tworkflow,
            paste0(x[[1, 1]], "_monthly_mean.csv")
          ),
          row.names = FALSE
        )
      } else {
        ifelse(
          i == 1,
          dm_t <- dm,
          dm_t <- rbind(dm_t, dm)
        )
      } # end if condition
    } # end i loop
    # --------------------------------------------------------------------------- #
    if (rbind == TRUE){
      if (save_output == TRUE){
        utils::write.csv(
          dm_t,
          file.path(
            envrmt$path_tworkflow,
            "all_monthly_means.csv"
          ),
          row.names = FALSE
        )
      } # end save output
      return(dm_t)
    } # end if condition
  } # end monthly loop
# --------------------------------------------------------------------------- #
# Method == "daily"
  if (method == "daily"){
    csv_list <- list()
    all_files_in_distribution <- list.files(
      path = envrmt$path_tworkflow,
      recursive = T
    ) # reads all data in Input-Folder

    csv_paths <- grep(
      "_no_NAs.csv$",
      all_files_in_distribution,
      value = TRUE
    )
    number_of_csvs <- length(csv_paths)

    for (i in 1:number_of_csvs){
      x <- utils::read.csv(
        file.path(
          envrmt$path_tworkflow,
          csv_paths[i]
        )
      )
      cn_x <- colnames(x)
      number_of_cn <- length(cn_x)

      for (j in 6:number_of_cn){
        fo <- stats::as.formula(
          paste(cn_x[j], " ~ day + month + year")
        )
        y <- stats::aggregate(fo, x, mean)
        colnames(y) <- c(
          "day",
          "month",
          "year",
          paste0(
            "daily_mean_",
            cn_x[j]
          )
        )
        y$plot <- x[[1, 1]]

# Add datetime for spacetimefolds later
        y$datetime <- with(y, ISOdate(year, month, day))
        y <- y[,
               c(
                 "plot",
                 "year",
                 "month",
                 "day",
                 paste0(
                   "daily_mean_",
                   cn_x[j]
                 )
               )
        ]

        ifelse(j == 6,
               dm <- y,
               dm <- cbind(dm, y[5])
        )
      } # end j loop

      if (rbind == FALSE){
        utils::write.csv(
          dm,
          file.path(
            envrmt$path_tworkflow,
            paste0("csv_", i, "_dm.csv")
          ),
          row.names = FALSE
        )
      } else {
        ifelse(
          i == 1,
          dm_t <- dm,
          dm_t <- rbind(dm_t, dm)
        )
      } # end if condition
    } # end i loop
    # --------------------------------------------------------------------------- #
    if (rbind == TRUE){
      if (save_output == TRUE){
        utils::write.csv(
          dm_t,
          file.path(
            envrmt$path_tworkflow,
            "all_daily_means.csv"
          ),
          row.names = FALSE
        )
      } # end save output
      return(dm_t)
    } # end if condition
  } # end daily loop
} # end function


#_____________________________________________________________________________#
#_____________________________________________________________________________#
#' Spatial aggregation for CSV-Data
#'
#' Extract station coordinates from meta-data and reproject the coordinates to the
#' project coordinate reference system.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method character. Either "daily", monthly" or "annual". Also depends on the available data.
#' @param des_file character. The filename and data type of the meta-data. (Only reads .csv)
#' @param crs character. EPSG of the Coordinate Reference System, if no **res_area.tif** file is provided.
#' @param save_output logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE.
#' Otherwise the output will be saved in the temporary directory. Default: TRUE
#' @param ...      arguments passed down from other functions.
#'
#' @return Data Frame
#' @seealso `prep.csv`, `proc.csv`, `fin.csv`
#'
#' @name spat.csv
#' @export spat.csv
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' #prepare csv-files
#' prep.csv(envrmt = envrmt,
#'          method = "proc",
#'          save_output = TRUE)
#'
#' #process csv-files
#' csv_data <- proc.csv(envrmt = envrmt,
#'                      method = "monthly",
#'                      rbind = TRUE,
#'                      save_output = TRUE)
#'
#' #extract station coordinates
#' csv_spat <- spat.csv(envrmt = envrmt,
#'                      method = "monthly",
#'                      des_file = "plot_description.csv",
#'                      save_output = TRUE)
#' head(csv_spat)
#'
spat.csv <- function(envrmt = .GlobalEnv$envrmt,
                     method = "monthly",
                     des_file,
                     crs = NULL,
                     save_output = TRUE,
                     ...){

  data <- utils::read.csv(
    file.path(
      envrmt$path_tworkflow,
      paste0(
        "all_",
        method,
        "_means.csv"
        )
      ),
    row.names = NULL
    )
  cn_data <- colnames(data)
  names_of_stations <- as.vector(
    unlist(
      unique(
         data[1]
        )
      )
    )
  number_of_stations <- length(names_of_stations)

# read plot description file
  des <- utils::read.csv(
    file.path(
      envrmt$path_dep,
      des_file
      )
    )

  data$lat <- ""
  data$lon <- ""

  for (i in 1:number_of_stations){
    data$lat[which(data$plot == names_of_stations[i])] <- des$lat[
      which(
        grepl(
          names_of_stations[i],
          des$plot
          )
        )
      ]

    data$lon[which(data$plot == names_of_stations[i])] <- des$lon[
      which(
        grepl(
          names_of_stations[i],
          des$plot
          )
        )
      ]
  } # end coordinate loop
# --------------------------------------------------------------------------- #
# Method = "monthly"
  if (method == "monthly"){
    monthly_means <- 5:length(cn_data)
    data[monthly_means] <- round(
      data[monthly_means],
      digits = 3
      )

    names(data)  <- c(
      names(data[1:4]),
      gsub(
        "monthly_mean_",
        "",
        names(data[monthly_means]
              )
        ),
      names(
        data[
          (5 + length(monthly_means)):length(names(data))
          ]
        )
      ) # end names

    if (is.null(crs)){
      crs <- terra::crs(
        terra::rast(
          file.path(
            envrmt$path_dep,
            "res_area.tif"
            )
          )
        )
      }

    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)

    points <- terra::vect(
      data,
      geom = c("lon", "lat"),
      crs = "+proj=longlat +datum=WGS84"
      )
    points <- terra::project(points, crs)
    data <- terra::as.data.frame(points, geom = "XY")

    utils::write.csv(
      data,
      file.path(
        envrmt$path_tworkflow,
        "spat_monthly_means.csv"
        ),
      row.names = FALSE
      )

    return(data)
  } # end monthly condition

# --------------------------------------------------------------------------- #
# Method = "daily"
  if (method == "daily"){
    daily_means <- 6:length(cn_data)
    data[daily_means] <- round(
      data[daily_means],
      digits = 3
      )
    names(data)  <- c(
      names(data[1:5]),
      gsub(
        "daily_mean_",
        "",
        names(data[daily_means])
        ),
      names(
        data[
          (6 + length(daily_means)):length(names(data))
          ]
        )
      ) # end names

    if (is.null(crs)){
      crs <- terra::crs(
        terra::rast(
          file.path(
            envrmt$path_dep,
            "res_area.tif"
            )
          )
        )
    }

    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)

    points <- terra::vect(
      data,
      geom = c("lon", "lat"),
      crs = "+proj=longlat +datum=WGS84"
    )
    points <- terra::project(points, crs)
    data <- terra::as.data.frame(points, geom = "XY")

    utils::write.csv(
      data,
      file.path(
        envrmt$path_tworkflow,
        "spat_daily_means.csv"
        ),
      row.names = FALSE
      )
    return(data)
  } # end daily condition

# --------------------------------------------------------------------------- #
# method = "hourly"
  if (method == "hourly") {
    hourly_means <- 7:length(cn_data)
    data[hourly_means] <- round(
      data[hourly_means],
      digits = 3
      )

    names(data)  <- c(
      names(data[1:6]),
      gsub(
        "hourly_mean_",
        "",
        names(data[hourly_means])
      ),
      names(
        data[
          (7 + length(hourly_means)):ncol(data)
        ]
      )
    ) # end names

    if (is.null(crs)){
      crs <- terra::crs(
        terra::rast(
          file.path(
            envrmt$path_dep,
            "res_area.tif"
            )
          )
        )
      }

    points <- terra::vect(
      data,
      geom = c("lon", "lat"),
      crs = "+proj=longlat +datum=WGS84"
    )
    points <- terra::project(points, crs)
    data <- terra::as.data.frame(points, geom = "XY")

    utils::write.csv(
      data,
      file.path(
        envrmt$path_tworkflow,
        "spat_hourly_means.csv"
        ),
      row.names = FALSE
      )
    return(data)
  }
}

# ___________________________________________________________________________ #
# ___________________________________________________________________________ #
#' Final aggregation for CSV-Data
#'
#' Extract the raster values of all raster layers from a scene at the station
#' coordinates at each time stamp. The extracted data will be attached to the
#' station data so there is a .csv-file with coordinates, sensor data (response values)
#' and extracted raster data (predictor values). The data is ready to be used for modelling.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param x Data frame. Climate station produced by spat.csv. Or produced via prepClimateStation. If null, climate statioon data is searched in Workflow/tworkflow.
#' @param method character. Either "daily", monthly" or "annual". Also depends on the available data.
#' @param save_output logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE.
#' Otherwise the output will be saved in the temporary directory. Default: FALSE.
#' @param crs character. If null, coordinate reference system from project files will be taken. Otherwise
#'            data will be reprojected into this crs.
#' @param ...      arguments passed down from other functions.
#'
#' @return List
#' @seealso `prep.csv`, `proc.csv`, `spat.csv`, `calc.indices`
#'
#' @name fin.csv
#' @export fin.csv
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # Load the climodr example data into the current climodr environment
#' clim.sample(envrmt = envrmt)
#'
#' #prepare csv-files
#' prep.csv(envrmt = envrmt,
#'          method = "proc",
#'          save_output = TRUE)
#'
#' #process csv-files
#' csv_data <- proc.csv(envrmt = envrmt,
#'                      method = "monthly",
#'                      rbind = TRUE,
#'                      save_output = TRUE)
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
#' #extract station coordinates
#' csv_spat <- spat.csv(envrmt = envrmt,
#'                      method = "monthly",
#'                      des_file = "plot_description.csv",
#'                      save_output = TRUE)
#'
#'
#' #extract predictor values from raster files
#' csv_fin <- fin.csv(envrmt = envrmt,
#'                    method = "monthly",
#'                    save_output = TRUE)
#' head(csv_fin)
#'

fin.csv <- function(envrmt = .GlobalEnv$envrmt,
                    x = NULL,
                    method = "monthly",
                    crs = NULL,
                    save_output = TRUE,
                    ...){

  if(!is.null(x)){
    data_o <- x
  } else {
    if (method == "monthly"){
      data_o <- utils::read.csv(
        file.path(
          envrmt$path_tworkflow,
          "spat_monthly_means.csv"
        )
      )
    } else {
      try(
        data_o <- utils::read.csv(file.path(
          envrmt$path_tworkflow,
          "Aggregated_Climate_Station_Data.csv"
        )),
        silent = TRUE
      )
    }
  }


  data_o$ID <- seq(1:nrow(data_o))

  all_files_in_distribution <- list.files(
    path = file.path(envrmt$path_rfinal),
    recursive = T
    ) # reads all data in Workflow Raster Folder

  sat_paths <- grep(
    "_ind.tif$",
    all_files_in_distribution,
    value = TRUE
    ) # Select tiff-files
  dgm_path <- grep(
    "_dgm_",
    all_files_in_distribution,
    value = TRUE
    ) # Select dgm

  if (method == "monthly"){
    if(file.exists(file.path(envrmt$path_rfinal, dgm_path))){
      dgm <- terra::rast(
        file.path(
          envrmt$path_rfinal,
          dgm_path
        )
      )
    }

    extr_total <- data.frame()

    for (i in 1:length(sat_paths)){
      month <- as.character(
        stringr::str_sub(
          gsub(
            ".*?([0-9]+).*",
            "\\1",
            sat_paths[i]
            ),
          5, 6)
        )
      tiff <- terra::rast(
        file.path(
          envrmt$path_rfinal,
          sat_paths[i]
          )
        )
      data_sub <- data_o[
        which(data_o$month == as.numeric(month)),]

      extr <- terra::extract(
        tiff,
        data.frame(
          x = data_sub$x,
          y = data_sub$y)
        );
      extr$ID <- data_sub$ID
      extr_total <- rbind(extr_total, extr)
    } # end i loop
    if(exists(dgm)){
      extr_dgm <- terra::extract(
        dgm,
        data.frame(x = data_o$x,
                   y = data_o$y
        )
      )
      extr <- merge(
        extr_dgm,
        extr_total,
        by = "ID"
      )
    }

    data <- merge(
      data_o,
      extr,
      by = "ID"
      )
    data$ID <- NULL
    data_n <- tidyr::drop_na(data)

    utils::write.csv(
      data_n,
      file.path(
        envrmt$path_tfinal,
        "final_monthly.csv"
        ),
      row.names = FALSE
      )
    return(data_n)
  } # end monthly loop

  if (method == "anual"){
    tiff_list <- list()
    for (i in 1:length(sat_paths)){
      tiff_list[[i]] <- terra::rast(file.path(envrmt$path_rworkflow, tiff_list[[i]]))
      if (i == 1){
        tiff_stack <- tiff_list[[i]]
      } else {
        terra::add(tiff_stack) <- tiff_list[[i]]
      }
    }

    data$ID <- seq(1:length(data[,1]))
    extr <- terra::extract(tiff_stack,
                           data.frame(x = data$lon,
                                      y = data$lat)
    )
    data <- merge(data, extr, by = "ID")
    data$ID <- NULL
    data$datetime <- NULL
    data_n <- tidyr::drop_na(data)

    utils::write.csv(data_n, file.path(envrmt$path_tfinal, "final_anualy.csv"), row.names = FALSE)
    return(data)
  } # end anual loop
} # end function
