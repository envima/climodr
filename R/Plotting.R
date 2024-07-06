#' Plotting
#'
#' Plot results of climodr into maps
#'
#' @param
#'
#' @return
#' @seealso
#'
#' @name climplot
#' @export climplot
#'
#' @examples
#'

climplot <- function(
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
    print(paste0("Creating maps for sensor [", sensor[s], "]."))

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
          terra::plot(AOA, add = TRUE)
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
