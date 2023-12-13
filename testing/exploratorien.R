devtools::load_all()

### Initiate climodr ###
# at home:
climodr::envi.create("E:/climodr/exploratories")

# in Lab II:
climodr::envi.create("F:/users/kluga/exploratories")

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
climodr::aggregate.rast()

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
                    autocorrelation = TRUE,
                    doParallel = TRUE)

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
