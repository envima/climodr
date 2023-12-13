devtools::load_all()

### Initiate climodr ###
climodr::envi.create("E:/climodr/exploratories")

### getting the data ###

# if(!require('remotes')) install.packages("remotes")
# if(!require('httr')) install.packages('httr')
# remotes::install_github('environmentalinformatics-marburg/tubedb/rTubeDB')

library(rTubeDB)
library(dplyr)
tubedb <- rTubeDB::TubeDB(url = "http://137.248.186.133:61036",
                          user = "alexander.klug",
                          password = "p4ybtxO4")

regionDF <- rTubeDB::query_regions(tubedb)
plotDF <- rTubeDB::query_region_plots(tubedb, "BE")
sensorDF <- rTubeDB::query_region_sensors(tubedb, "BE")

# creating meta data
plotDF.hainich <- plotDF %>% filter(plotDF$general_station %in% c("HEF", "HEG", "HEW"))
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

# --------------------------------------------------------------------------- #
### Pre-Processing ###

# define areo of interest and coordinate reference system
aoi <- terra::vect(file.path(envrmt$path_vector, "exploratorium_hainich.gpkg"))
coord.sys <- terra::crs("EPSG:25832")

## --- Prepare Raster Data --- ##

# crop rasters and set coordinate reference system
climodr::crop.all(method = "MB_Timeseries",
                  ext = aoi,
                  crs = coord.sys)
# calculate indices

climodr::calc.indices

# mean rasters

all_files <- list.files(path = file.path(envrmt$path_rfinal), recursive = T); #reads all data in Workflow Raster Folder
dates <- as.Date(stringr::str_sub(all_files, 5, 12), format = "%Y%m%d")
months <- strftime(dates, format = "%m")

for (i in unique(months)[10:12]){
  print(paste0("Calculating means for ",
               unique(strftime(dates, format = "%B"))[as.integer(i)],
               "..  ",
               i, "/", length(unique(months))))
  month_rasters <- all_files[which(months == i)]
  rstack <- terra::rast()
  rmean <- terra::rast()

  print(paste0("Reading ", length(month_rasters), " files for this month..  "))

  for (j in 1:length(month_rasters)){
    x <- terra::rast(file.path(envrmt$path_rfinal, month_rasters[j]))
    terra::add(rstack) <- x
  } # end j-loop

  n <- terra::nlyr(rstack)
  l <- length(terra::sources(rstack))
  vsort <- names(rstack)[1:(n/l)]
  rstack <- rstack[[sort(names(rstack))]]

  print("Calculate monthly means for each layer.. ")
  for (k in seq(1, n, l)){
    print("  ...  ")
    a <- rstack[[k:(k+l-1)]]
    b <- terra::app(a, fun = "mean")
    names(b) <- names(a)[1]
    terra::add(rmean) <- b
  } # end k-loop

  print("Writing raster..")
  terra::writeRaster(rmean,
                     file.path(envrmt$path_rfinal,
                               paste0("hai_2020",
                                      unique(months)[as.integer(i)],
                                      "_mean.tif")
                               )
                     )
  remove(rstack, rmean, j, n, l, vsort, k, a, b)
  gc()
} # end i-loop

data_order <- names(terra::rast(file.path(envrmt$path_rworkflow, "hai_20200104_ind.tif")))
all_files <- list.files(path = file.path(envrmt$path_rfinal), recursive = T)

for (i in 1:12){
  data <- terra::rast(file.path(envrmt$path_rfinal, all_files[i]))
  data <- data[[data_order]]
  data$EVI <- NULL
  data$EVI2 <- NULL
  print(i)
  terra::writeRaster(data, file.path(envrmt$path_rfinal, "new", all_files[i]))
}

# --------------------------------------------------------------------------- #

# clean data
climodr::prep.csv(method = "proc", safe_output = TRUE)

# aggregate data to monthly means
csv_data <- climodr::proc.csv(method = "all",
                              rbind = TRUE,
                              safe_output = TRUE)
head(csv_data)

# make csv data spatial
csv_spat <- climodr::spat.csv(method = "monthly",
                              des_file = "plot_description.csv",
                              crs = coord.sys,
                              safe_output = TRUE)
head(csv_spat)

# finalize csv data
csv_fin <- climodr::fin.csv(method = "monthly",
                            crs = coord.sys,
                            safe_output = TRUE)
head(csv_fin)

# --------------------------------------------------------------------------- #
### modelling ###


eval_vector <- c(5, 6, 9:47)
climodr::autocorr(eval_vector, plot.corrplot = FALSE)

# Modelling
climodr::calc.model(timespan = c(2020),
                    climresp = c(5,6),
                    classifier = c("rf", "pls" ,"lm", "gbm"),
                    seed = 707,
                    p = 0.8,
                    folds = "all",
                    mnote = "f20run",
                    predrows = c(9:47),
                    tc_method = "cv",
                    metric = "RMSE",
                    autocorrelation = TRUE)

# Prediction
mod_tem <- readRDS(file.path(envrmt$path_models, "20201_LLO_normal_raf_tem_ffs_model.rds"))
mod_reh <- readRDS(file.path(envrmt$path_models, "20201_LLO_normal_raf_reh_ffs_model.rds"))
tif <- terra::rast(file.path(envrmt$path_rfinal, "hai_202001_mean.tif"))
dgm <- terra::rast(file.path(envrmt$path_rfinal, "hai_dgm.tif"))
dgm <- terra::resample(dgm, tif)
terra::add(tif) <- dgm

pred_tem <- terra::predict(tif, mod_tem, na.rm = T)
pred_reh <- terra::predict(tif, mod_reh, na.rm = T)
terra::writeRaster(pred_tem, file.path(envrmt$path_preds, "hai_202001_tem_pred.tif"))
terra::writeRaster(pred_reh, file.path(envrmt$path_preds, "hai_202001_reh_pred.tif"))

aoa_tem <- CAST::aoa(tif, mod_tem)
aoa_reh <- CAST::aoa(tif, mod_reh)
terra::writeRaster(aoa_tem$AOA, file.path(envrmt$path_preds, "hai_202001_tem_aoa.tif"))
terra::writeRaster(aoa_reh$AOA, file.path(envrmt$path_preds, "hai_202001_reh_aoa.tif"))

terra::plot(pred)
terra::plot(aoa$AOA, add = TRUE)
