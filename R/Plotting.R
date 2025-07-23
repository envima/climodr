#' Create Maps using the `terra` package graphic parameters
#'
#' Plot results of climodr into maps. Right now maps are created using the terra package.
#' The maps created are very basic. Will be updated to run with tidyterra in future.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param mnote character. The modelnote you want to create maps of.
#' @param sensor character. The sensor you want to create maps for.
#' @param aoa logical. Do you want the area of applicability to be added to your map?
#' @param mapcolors The color pallete you want to use for the map. Default is `rev(grDevices::terrain.colors(50))`
#' @param scale_position character. Graphical parameter. The relative positiion of the Scale for the map. See `terra::plot` for more details.
#' @param north_position character. Graphical parameter. The relative positiion of the Scale for the map. See `terra::plot` for more details.
#'
#' @return Maps in PNG-Format to your harddrive.
#' @seealso `terra::plot`
#'
#' @name climplot
#' @export climplot
#'
#' @importFrom grDevices adjustcolor colorRampPalette dev.off png
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
#'          metric = "Nrmse",
#'          mnote = "normal",
#'          AOA = TRUE)
#'
#' # Create a Temperature Map from the vignette model
#' climplot(envrmt = envrmt,
#'          mnote = "normal",
#'          sensor = "Ta_200",
#'          aoa = TRUE,
#'          mapcolors = rev(heat.colors(50)),
#'          scale_position = "bottomleft",
#'          north_position = "topright")
#' }

climplot <- function(
    envrmt = .GlobalEnv$envrmt,
    mnote,
    sensor,
    aoa = FALSE,
    mapcolors = rev(grDevices::terrain.colors(50)),
    scale_position = "bottomleft",
    north_position = "topright"){

  all_preds <- list.files(
    envrmt$path_predictions,
    pattern = ".tif",
    recursive = FALSE)

  for (s in 1:length(sensor)){
    preds <- all_preds[which(grepl(all_preds, pattern = paste0(mnote, "_", sensor[s])))]
    message(paste0("Creating maps for sensor [", sensor[s], "]."))

    for (i in 1:length(preds)){
      filename <- gsub("_prediction.tif.*", "", preds[i])
      prediction <- terra::rast(file.path(envrmt$path_predictions, preds[i]))
      if(isTRUE(aoa)){
        AOA <- terra::rast(file.path(envrmt$path_aoa, paste0(filename, "_aoa.tif")))
      }

      if ((terra::xmax(prediction) - terra::xmin(prediction)) < 10000){
        scale <- 1
        scale_label <- "meters"
      } else {
        scale <- 1000
        scale_label <- "kilometers"}

      png(filename = file.path(envrmt$path_maps, paste0("map_", filename, ".png")))
        terra::plot(
          main = filename,
          prediction,
          col = mapcolors,
          background = "lightgrey",
          box = TRUE)
        if(isTRUE(aoa)){
          ac <- data.frame(ac_code = c(0, 1),
                           ac_class = c("applicable",
                                        "not applicable"))
          levels(AOA) <- ac
          terra::plot(AOA,
                      col = c(adjustcolor("white", alpha.f = 0),
                              "grey"),
                      legend = "bottomright",
                      add = TRUE)

        }
        terra::sbar(
          scaleby = scale,
          xy = scale_position,
          type = "bar",
          divs = 4,
          below = scale_label)
        terra::north(
          xy = north_position,
          type = 2)
      dev.off()
    }
  } # end sensor loop
}
