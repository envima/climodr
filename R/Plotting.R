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
#' \dontrun{
#' # Create a Temperature Map from the vignette model
#' climplot(mnote = "vignette",
#'          sensor = "Ta_200",
#'          aoa = FALSE,
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
