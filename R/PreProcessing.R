#' Preparing Climate Station Data
#'
#' Crops input data to the extent size and removes NA-Values
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param x dataframe or path. A data frame containing climate station data or one of the Input paths of the envrmt (either envrmt$path_tabular or envrmt$path_vector)
#' @param pattern character. Some string indicating that file is a climate station. E.g. "Climate_Station_*.csv" All climate station files in input folder must contain this pattern in filename.
#' @param metadata vector. 5 Entries. Name of your metadata-file in your Input/dep folder, the column name for the climate stations, the name of your X and Y columns and the coordinate reference system. (e.g. c("metadata.csv", "plotID", "lon", "lat", "+proj=longlat +datum=WGS84"))
#' @param time_column character. The name of the time column in your climate station data.
#' @param time_format character. How is your time column formated? (e.g. YYYY-MM-DD)
#' @param station_ids character. The name of the station id column of your climate station data.
#' @param sensors vector. Containing all column names of each sensor in your climate station data.
#' @param aggregation_interval character. To what format should your data be aggregated? Same as units in [lubridate::floor_date()].
#' @param start character. When do you want your climate station data to start?
#' @param end character. When do you want your climate station data to end?
#' @param crs character. Destined coordinate reference system. If crs = NULL, climod searches for "res_area.tif" in Input/dep.
#' @param sep character. Which seperator is used for the columns in the climate station data? (Default = ",")
#' @param dec character. Which symbol is used to mark decimal digits? (Default = ".")
#' @param ... arguments passed down from other functions.
#'
#' @return data frame with aggregated climate station data
#' @seealso replacement function for the old [prep.csv], [proc.csv], [spat.csv] functions.
#'
#' @name prepClimateStations
#' @export prepClimateStations
#'
#' @examples
#' # create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # load example data
#' climsample(envrmt = envrmt)
#'
#' # prepare climate station data
#' climdata <- prepClimateStations(envrmt = envrmt,
#'                                 x = envrmt$path_tabular,
#'                                 pattern = "Station",
#'                                 metadata = c("plot_description.csv",
#'                                              "plot",
#'                                              "lon",
#'                                              "lat",
#'                                              "+proj=longlat +datum=WGS84"),
#'                                 time_column = "datetime",
#'                                 time_format = "%Y-%m-%dT%H",
#'                                 station_ids = "plotID",
#'                                 sensors = "Ta_200",
#'                                 aggregation_interval = "month")
#' head(climdata)

prepClimateStations <- function(envrmt = .GlobalEnv$envrmt,
                                x,
                                pattern = NULL,
                                metadata = NULL,
                                time_column,
                                time_format,
                                station_ids,
                                sensors,
                                aggregation_interval = NULL,
                                start = NULL,
                                end = NULL,
                                crs = NULL,
                                sep = ",",
                                dec = ".",
                                save_output = TRUE){
  # start reading climate stations
  if(!is.data.frame(x) & x != envrmt$path_tabular & x != envrmt$path_vector){
    stop("Something is wrong with x. Make sure it is either a data frame with climate station data or a input destination of the climodr environment made with envi.create().")
  }
  # either take data frame
  if(x != envrmt$path_tabular & x != envrmt$path_vector){
    data_o <- x

  # or read all possible climate stations from folders
  } else {
    all_stations <- list.files(x, pattern = pattern)
    data_o <- readClimateData(station_list = all_stations,
                              folder = x,
                              station_ids = station_ids,
                              metadata = metadata,
                              sep = sep, dec = dec)
    if(!is.data.frame(data_o)){
      metadata <- data_o$metadata
      data_o <- data_o$stations
    }
  }

  # time step aggregation of climate data
  data_o[time_column] <- as.POSIXct(as.vector(unlist(data_o[time_column])), format = time_format)

  # subset for start and end
  if(!is.null(start) & !is.null(end)){
    time_lim <- as.POSIXct(c(start, end), format = time_format)
    data_o <- subset(data_o, time_lim[1] <= data_o[, time_column] & data_o[, time_column] <= time_lim[2])
  } else {
    if(!is.null(start) & is.null(end)){
      time_lim <- as.POSIXct(start, format = time_format)
      data_o <- subset(data_o, time_lim <= data_o[, time_column])
    }
    if(is.null(start) & !is.null(end)){
      time_lim <- as.POSIXct(end, format = time_format)
      data_o <- subset(data_o, data_o[, time_column] <= time_lim)
    }
  } # end subset for start and end

  if(!is.null(aggregation_interval)){
    if(!aggregation_interval %in% c("second", "minute", "hour", "day", "week", "month", "bimonth", "quarter", "season", "halfyear", "year")){
      stop("Invalid aggregation interval. Check unit argument in [lubridate::floor_date()] to see valid inputs.")
    }
    data_o[time_column] <- lubridate::floor_date(data_o[, time_column], aggregation_interval)
    data_agg <- aggregate_sensor(data = data_o,
                                 sensors = sensors,
                                 time_column = time_column,
                                 station_ids = station_ids,
                                 min_entries = 1,
                                 coords = c("x", "y"))
  } else {
    data_agg <- data_o
  }

  # Note: For better aggregation, aggregate in time steps (e.g. daily -> monthly -> yearly)
  # Material for future update. Right now, data quality is entrustet by the user.

  # projection into correct project crs

  points <- terra::vect(data_agg,
                        geom = c("x", "y"),
                        crs = metadata[5])
  if(is.null(crs)){
    crs <- terra::crs(terra::rast(file.path(envrmt$path_dep, "res_area.tif")))
  }

  points <- terra::project(points, crs)
  data_proj <- terra::as.data.frame(points, geom = "XY")

  # save and return
  data_fin <- splitTime(data_proj,
                        time_column = time_column,
                        smallest_interval = aggregation_interval)

  if(isTRUE(save_output)){
    utils::write.csv(data_fin,
                     file.path(envrmt$path_tworkflow, "Aggregated_Climate_Station_Data.csv"),
                     row.names = FALSE)
  }
  return(data_fin)
}

#' Read climate station data
#'
#' Readable climate station data for now: .csv, .txt, .gpkg, .shp
#'
#' @param station_list list. A list containing all paths of each climate station that should be read.
#' @param folder path. Folder where all the climate station data is stored in. Specifically made for climodr-environment. Either "$path_tabular" or "$path_vector".
#' @param station_ids character. The name of the station id column of your climate station data.
#' @param metadata vector. 5 Entries. Name of your metadata-file in your Input/dep folder, the column name for the climate stations, the name of your X and Y columns and the coordinate reference system. (e.g. c("metadata.csv", "plotID", "lon", "lat", "+proj=longlat +datum=WGS84")). NULL if data is vector_data.
#' @param sep character. Which seperator is used for the columns in the climate station data? (Default = ",")
#' @param dec character. Which symbol is used to mark decimal digits? (Default = ".")
#'
#' @return data frame or list. Depends if climate stations are vector files or tabular files. Tabular files return a single data frame, vector files return a list with the same data frame and a metadata vector, containting information about the metadata lost in conversion.
#' @seealso [prepClimateStations]
#'
#' @name readClimateData
#' @export readClimateData
#'
#' @examples
#' # create climodr environment and allow terra-functions to use 70% of RAM
#' envrmt <- envi.create(proj_path = tempdir(),
#'                       memfrac = 0.7)
#'
#' # load example data
#' climsample(envrmt = envrmt)
#'
#' # read climate station data
#' climate_stations <- list.files(envrmt$path_tabular, pattern = "Station")
#' csd <- readClimateData(station_list = climate_stations,
#'                        folder = envrmt$path_tabular,
#'                        station_ids = "plotID",
#'                        metadata = c("plot_description.csv",
#'                                     "plot",
#'                                     "x",
#'                                     "y",
#'                                     "+proj=longlat +datum=WGS84"))
#' head(csd)

readClimateData <- function(station_list,
                            folder,
                            station_ids,
                            metadata = NULL,
                            sep = ",",
                            dec = "."){

  # Scan input folder for climate stations
  formats <- sub(".*[.]", "", station_list)
  formats <- formats[which(formats %in% c("csv", "txt", "gpkg", "shp"))]

  # warn the user if scanning finds more then one file type.
  if(length(unique(formats)) > 1){
    warning("More then one filetype detected while scanning for climate station data. Climate station files of different types might be arranged differently and cause errors.")
  }

  for (i in 1:length(station_list)){
    if(folder == envrmt$path_tabular){
      if(formats[i] == "csv"){
        data <- utils::read.csv(file.path(folder, station_list[i]), sep = sep, dec = dec)
      }
      if(formats[i] == "txt"){
        data <- utils::read.table(file.path(folder, station_list[i]), sep = sep, dec = dec, header = TRUE)
      }
    } # end tabular input

    if(folder == envrmt$path_vector){
      data <- terra::vect(file.path(folder, station_list[i]))
      crs <- terra::crs(data)
      data <- terra::as.data.frame(data, geom = "XY")
    } # end vector input

    # merge all climate stations to one data frame
    ifelse(i == 1,
           stations <- data,
           stations <- rbind(stations, data))
  }

  # add coordinates, if not existing already
  if(!is.null(metadata)){
    m_data <- utils::read.table(file.path(envrmt$path_dep, metadata[1]), sep = sep, dec = dec, header = TRUE, row.names = 1)
    stations$x <- ""
    stations$y <- ""
    unique_stations <- as.vector(unlist(unique(stations[station_ids])))
    for (j in 1:length(unique_stations)){
      stations$x[which(stations[station_ids] == unique_stations[j])] <-
        m_data[which(unique_stations[j] == m_data[metadata[2]]), metadata[3]]
      stations$y[which(stations[station_ids] == unique_stations[j])] <-
        m_data[which(unique_stations[j] == m_data[metadata[2]]), metadata[4]]
    }
  } else {
    if(folder == envrmt$path_vector){
      metadata <- c("Vectorfile", station_ids, "x", "y", crs)
      stations <- list(stations, metadata)
      names(stations) <- c("stations", "metadata")
    }
  }

  stations$x <- as.numeric(stations$x)
  stations$y <- as.numeric(stations$y)

  return(stations)
}

#' Aggregate Sensor
#'
#' Description
#'
#' @param data climate station data used to be aggregated
#' @param sensors character. Column names of your sensor columns.
#' @param station_ids character. Column name of your Climate Station IDs.
#' @param time_column character. The name of the time column in your climate station data.
#' @param min_entries minimum entries a time interval must have to be validly aggregated (e.g. 18 days for a monthly aggregation)
#' @param coords vector. If your climate station data already has coordinates, add coordinate column names in this argument (e.g. c("X", "Y"))
#'
#' @return data frame with aggregated climate station data
#' @seealso [prepClimateStations]
#'
#' @name aggregate_sensor
#' @export aggregate_sensor
#'
#' @examples
#' # example code
#'

aggregate_sensor <- function(data, sensors, station_ids, time_column, min_entries, coords = NULL){
  # create empty dataframe
  product <- data[0, ]

  # loop aggregation over sensors over plots
  for (i in 1:(length(sensors))){
    foo <- stats::as.formula(paste0(colnames(data[sensors[i]]), " ~ ", time_column))
    counter <- 1
    for (j in unique(data[, station_ids])){
      t <- table(data[which(data[, station_ids] == j), time_column])
      check <- data[which(data[, station_ids] == j), time_column] == names(t[which(t < min_entries)])
      data[which(data[, station_ids] == j), sensors][which(check)] <- NA

      try(data_agg <- stats::aggregate(foo,
                                       data[which(data[, station_ids] == j), ],
                                       mean,
                                       na.rm = TRUE),
          silent = TRUE)
      product[c(counter:(nrow(data_agg) + counter - 1)), station_ids] <- j
      product[c(counter:(nrow(data_agg) + counter - 1)), c(time_column, sensors[i])] <- data_agg
      if(!is.null(coords)){
        product[c(counter:(nrow(data_agg) + counter - 1)), coords] <- data[which(data[, station_ids] == j)[1], coords]
      }
      counter <- counter + nrow(data_agg)
    }
  }
  return(product)
}

#' Split Time
#'
#' Description
#'
#' @param data Data frame. Climate station data with time column in POSIXct-format
#' @param time_column Character. Column name of the column containing timestamps in POSIXct-format
#' @param smallest_interval The smallest time intervall the data should be split into. Same as units in [lubridate::floor_date()]
#'
#' @return data frame with aggregated climate station data
#' @seealso [prepClimateStations]
#'
#' @name splitTime
#' @export splitTime
#'
#' @examples
#' # example code
#'

splitTime <- function(data, time_column, smallest_interval){
  # identify time column
  tc <- which(names(data) == time_column)

  # manual entry of time columns
  if(smallest_interval == "year"){
    cols <- "year"
  }
  if(smallest_interval %in% c("halfyear", "season", "quarter", "bimonth", "month")){
    cols <- c("year", "month")
  }
  if(smallest_interval %in% c("week", "day")){
    cols <- c("year", "month", "day")
  }
  if(smallest_interval == "hour"){
    cols <- c("year", "month", "day", "hour")
  }
  if(smallest_interval == "minute"){
    cols <- c("year", "month", "day", "hour", "minute")
  }
  if(smallest_interval == "second"){
    cols <- c("year", "month", "day", "hour", "minute", "second")
  }

  # format vector fitting to i
  formats <- c("%Y", "%m", "%d", "%H", "%M", "%S")
  times <- as.data.frame(matrix(nrow = nrow(data), ncol = length(cols)))
  names(times) <- cols

  # loop for date splitting
  for(i in 1:length(cols)){
    times[,i] <- as.character(format(data[, time_column], format = formats[i]))
  }

  # merge everything together
  data_split <- cbind(data[, 1:tc], times, data[(tc+1):ncol(data)])
  return(data_split)
}
