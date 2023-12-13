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

# if(!require('remotes')) install.packages("remotes")
# if(!require('httr')) install.packages('httr')
# remotes::install_github('environmentalinformatics-marburg/tubedb/rTubeDB')

# library(rTubeDB)
# library(dplyr)
# tubedb <- rTubeDB::TubeDB(url = "http://137.248.186.133:61036",
#                           user = "alexander.klug",
#                           password = "my password")

# regionDF <- rTubeDB::query_regions(tubedb)
# plotDF <- rTubeDB::query_region_plots(tubedb, "BE")
# sensorDF <- rTubeDB::query_region_sensors(tubedb, "BE")

# creating meta data
# plotDF.hainich <- plotDF %>% filter(plotDF$general_station %in% c("HEF", "HEG", "HEW"))
# plotDF.hainich <- plotDF.hainich[1:103, -c(4)]
# names(plotDF.hainich) <- c("plot", "general_station", "logger_type", "lat", "lon", "elevation")
# write.csv(plotDF.hainich,
#           file.path(envrmt$path_dep, "plot_description.csv"),
#           row.names = FALSE,
#           quote = FALSE)

# download data
# data_o <- rTubeDB::query_timeseries(tubedb,
#                                     plot = row.names(plotDF.hainich),
#                                     sensor = c("Ta_200",
#                                                "rH_200"),
#                                     datetimeFormat = "POSIXlt")
# saving raw data
# write.csv(data_o, row.names = FALSE, (file.path(envrmt$path_tmp, "hainich_raw.csv")))

# splitting up stations (for now)
# stations <- unique(data_o$plot)
# for (i in 1:length(stations)){
#   data <- data_o[which(data_o$plot == stations[i]), ]
#   write.csv(data,
#             row.names = FALSE,
#             file.path(envrmt$path_tabular,
#                       paste0(stations[i], ".csv")
#             )
#   )
# }

# remove(data, data_o, stations, i, regionDF, sensorDF, plotDF, plotDF.filtered, tubedb)
# gc()
