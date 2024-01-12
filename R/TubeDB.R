#' TubeDB
#'
#' TubeDB integration
#'
#' @param
#'
#' @return
#' @seealso
#'
#' @name
#' @export
#'
#' @examples
#'

get.data <- function(method = "tubeDB",
                     data_url = NULL,
                     user = NULL,
                     password = NULL,
                     region = "NULL"){
  library(rTubeDB)
  library(dplyr)

  tubedb <- rTubeDB::TubeDB(url = data_url,
                            user = user,
                            password = password)

  regionDF <- rTubeDB::query_regions(tubedb)
  plotDF <- rTubeDB::query_region_plots(tubedb, "BE")
  sensorDF <- rTubeDB::query_region_sensors(tubedb, "BE")

  if (region == "Hainich"){
    stations <- c("HEF", "HEG", "HEW")
  }

  if (region == "Schorfheide"){
    stations <- c()
  }

  if (region == "Schwäbische Alb"){
    stations <- c()
  }

  # creating meta data
  plotDF.hainich <- plotDF %>% filter(plotDF$general_station %in% stations)
  plotDF.hainich <- plotDF.hainich[1:103, -c(4)]
  names(plotDF.hainich) <- c("plot", "general_station", "logger_type", "lat", "lon", "elevation")
  write.csv(plotDF.hainich,
            file.path(envrmt$path_dep, "plot_description.csv"),
            row.names = FALSE,
            quote = FALSE)

  # download data
  data_o <- rTubeDB::query_timeseries(tubedb,
                                      plot = row.names(plotDF.hainich),
                                      sensor = c("Ta_200",
                                                 "rH_200"),
                                      datetimeFormat = "POSIXlt")
  # saving raw data
  write.csv(data_o, row.names = FALSE, (file.path(envrmt$path_tmp, "hainich_raw.csv")))

  # splitting up stations (for now)
  stations <- unique(data_o$plot)
  for (i in 1:length(stations)){
    data <- data_o[which(data_o$plot == stations[i]), ]
    write.csv(data,
              row.names = FALSE,
              file.path(envrmt$path_tabular,
                        paste0(stations[i], ".csv")
              )
    )
  }

  remove(data, data_o, stations, i, regionDF, sensorDF, plotDF, plotDF.filtered, tubedb)
  gc()
}
