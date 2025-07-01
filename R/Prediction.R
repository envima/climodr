#' Predict sensor data area wide
#'
#' Use the models created using `calc.model` to predict the modeled data onto a
#' full spatial raster scene.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method Character. Either "daily", monthly" or "annual". Also depends on the available data.
#' @param metric Character. Which perfomance metric should be used to determine the best model? One of "accuracy", "Nrmse" or "Rsqrd". Default = "accuracy".
#' @param mnote Character. Model note to filter models for the fitting model run.
#' @param AOA Logical. Should the Area of Applicability be calculated additional to the models?
#'
#' @return Multiple models.rds stored in the /workflow/models folder.
#' @seealso `autocorr`, `predict`
#'
#' @name climpred
#' @export climpred
#'
#' @examples
#' \donttest{
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
#'
#' # Test data for autocorrelation after running fin.csv
#' autocorr(envrmt = envrmt,
#'          method = "monthly",
#'          resp = 5,
#'          pred = c(8:23),
#'          plot.corrplot = FALSE)
#'
#' # Create 36 different models (12 months x 3 classifiers) for every month in 2017
#' calc.model(envrmt = envrmt,
#'            method = "monthly",
#'            timespan = c(2017),
#'            climresp = 5,
#'            classifier = c("rf",
#'                           "pls",
#'                           "lm"),
#'            seed = 707,
#'            p = 0.8,
#'            folds = "LLO",
#'            mnote = "normal",
#'            predrows = c(8:23),
#'            tc_method = "cv",
#'            metric = "RMSE",
#'            autocorrelation = TRUE,
#'            doParallel = FALSE)
#'
#' # Make predictions
#' climpred(envrmt = envrmt,
#'          method = "monthly",
#'          metric = "accuracy",
#'          mnote = "normal",
#'          AOA = TRUE)
#'
#' predlist <- list.files(envrmt$path_predictions,
#'                        pattern = ".tif")
#' head(predlist)
#' }
#'

climpred <- function(
    envrmt = .GlobalEnv$envrmt,
    method = "monthly",
    metric = "accuracy",
    mnote,
    AOA = TRUE){

  # create a list with all raster images
  tiff_list <- list.files(
    path = envrmt$path_rfinal,
    pattern = ".tif",
    recursive = TRUE
  )

  # read dgm
  dgm <- terra::rast(
    file.path(
      envrmt$path_rfinal,
      grep(
        pattern = "_dgm_",
        tiff_list,
        value = TRUE)
      )
    )

# read eval_df
  eval_df <- readRDS(
    file.path(
      envrmt$path_statistics,
      paste0(
        mnote,
        "_mod_eval_df.rds"
        )
      )
    )

# filter for best models
  dates <- unique(eval_df[, 1])
  for (i in 1:length(dates)){
    mod_date <- eval_df[which(eval_df[, 1] == dates[i]), ]
    ifelse(
      i == 1,
      mod_df <- mod_date[
        which(mod_date$Nrmse == min(mod_date$Nrmse)), ],
      mod_df[i, ] <- mod_date[
        which(mod_date$Nrmse == min(mod_date$Nrmse)), ]
      )
# read fitting raster
    raster <- terra::rast(
      file.path(
        envrmt$path_rfinal,
        grep(
          pattern = dates[i],
          tiff_list,
          value = TRUE
        )
      )
    )
    terra::add(raster) <- dgm

    modname <- paste0(
      mnote,
      "_",
      mod_df[i, ]$sensor,
      "_",
      dates[i],
      "_",
      mod_df[i, ]$modeltype,
      "_",
      mod_df[i, ]$classifier
    )
    mod <- readRDS(
      file.path(
        envrmt$path_models,
        paste0(modname,
               "_ffs_model.rds")
      )
    )

    message(
      paste0(
        "Making ",
        mod_df[i, ]$sensor,
        " ",
        mod_df[i, ]$classifier,
        "-prediction for ",
        dates[i],
        "."
        )
      )

    pred <- terra::predict(
      raster,
      mod,
      na.rm = TRUE
      )
    names(pred) <- modname
    terra::writeRaster(
      pred,
      file.path(
        envrmt$path_predictions,
        paste0(
          modname,
          "_prediction.tif"
          )
        ),
      overwrite = TRUE
    )

    if (isTRUE(AOA)) try({
      suppressWarnings(
        suppressMessages(
          aoa <- CAST::aoa(
            newdata = raster,
            model = mod)
        )
      )
      names(aoa$AOA) <- paste0(modname, "_aoa")
      terra::writeRaster(
        aoa$AOA,
        file.path(
          envrmt$path_aoa,
          paste0(
            modname,
            "_aoa.tif"
          )
        ),
        overwrite = TRUE
      )
    })
  } # end i loop

  for (i in 1:nrow(mod_df)){
    mod_df$variables[[i]] <- paste(
      mod_df$variables[[i]],
      collapse = ", "
      )
  }
  mod_df <- data.frame(mod_df)
  mod_df <- apply(mod_df, 2, as.character)

  utils::write.csv(
    mod_df,
    file.path(
      envrmt$path_statistics,
      paste0(
        mnote,
        "_best_models.csv"
      )
    ),
    row.names = FALSE
  )
} # end function loop
