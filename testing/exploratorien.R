devtools::load_all()

### Initiate climodr ###
climodr::envi.create("E:/climodr/exploratories/Schorfheide_100")

### getting the data ###

# if(!require('remotes')) install.packages("remotes")
# if(!require('httr')) install.packages('httr')
# remotes::install_github('environmentalinformatics-marburg/tubedb/rTubeDB')

climodr::get.data(method = "tubeDB",
                  data_url = "http://137.248.186.133:61036",
                  user = "alexander.klug",
                  password = "p4ybtxO4")

# --------------------------------------------------------------------------- #
### Pre-Processing ###

# define area of interest and coordinate reference system
aoi <- terra::vect(file.path(envrmt$path_vector, "be_sch_area.gpkg"))
coord.sys <- terra::crs("EPSG:32632")

## --- Prepare Raster Data --- ##

# crop rasters and set coordinate reference system
climodr::crop.all(method = "MB_Timeseries",
                  ext = aoi,
                  crs = coord.sys)

# calculate indices
climodr::calc.indices()

# --------------------------------------------------------------------------- #

# clean data
climodr::prep.csv(method = "proc", safe_output = TRUE)

# aggregate data to monthly means
csv_data <- climodr::proc.csv(method = "all",
                              rbind = TRUE,
                              safe_output = TRUE)
head(csv_data)            # month and year were copied, daily_mean_year.1

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
head(csv_fin)[, c(1:12)]

# --------------------------------------------------------------------------- #
### modelling ###
eval_vector <- c(5, 8:24)
climodr::autocorr(eval_vector, plot.corrplot = FALSE)

# Modelling
climodr::calc.model(timespan = c(2017),
                    climresp = c(5),
                    classifier = c("rf", "pls", "nnet", "lm"),
                    seed = 707,
                    p = 0.8,
                    folds = "all",
                    mnote = "sch100",
                    predrows = c(8:24),
                    tc_method = "cv",
                    metric = "RMSE",
                    autocorrelation = FALSE)

# eval_df
eval_df <- readRDS(file.path(envrmt$path_statistics, "sch100_mod_eval_df.rds"))

# Prediction
mod_tem <- readRDS(file.path(envrmt$path_models, "sch100_tem_20177_LLO_pls_ffs_model.rds"))

#mod_reh <- readRDS(file.path(envrmt$path_models, "f20run_reh_20207_LLO_pls_ffs_model.rds"))
tif <- terra::rast(file.path(envrmt$path_rfinal, "sch_201707_100_mean_ind.tif"))
dgm <- terra::rast(file.path(envrmt$path_raster, "sch_100_dgm.tif"))
dgm <- terra::resample(dgm, tif)
terra::add(tif) <- dgm

pred_tem <- terra::predict(tif, mod_tem, na.rm = T)
#pred_reh <- terra::predict(tif, mod_reh, na.rm = T)
terra::writeRaster(pred_tem, file.path(envrmt$path_predictions, "sch_201707_tem_pred.tif"), overwrite = TRUE)
#terra::writeRaster(pred_reh, file.path(envrmt$path_predictions, "hai_202007_reh_pred.tif"))

aoa_tem <- CAST::aoa(tif, mod_tem)
#aoa_reh <- CAST::aoa(tif, mod_reh)
terra::writeRaster(aoa_tem$AOA, file.path(envrmt$path_aoa, "hai_202007_tem_aoa.tif"))
#terra::writeRaster(aoa_reh$AOA, file.path(envrmt$path_aoa, "hai_202007_reh_aoa.tif"))

terra::plot(pred)
terra::plot(aoa$AOA, add = TRUE)

df <- readRDS(file.path(envrmt$path_statistics, "f20run_mod_eval_df.rds"))


points <- terra::vect(csv_fin,
                      geom = c("lon", "lat"),
                      crs = coord.sys)
terra::plot(aoi)
terra::plot(points,
            add = TRUE)

