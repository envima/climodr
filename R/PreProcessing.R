#' Cropping all data
#'
#' Crops input data to the extent size
#'
#' @param folder_path charackter. Path to input folder. All necessary and relevant data should
#' be stored here. Default: Input created by [climodr::hubs].
#' @param crs Coordinate reference system Used to crop all images in folder_path. If crs
#' it will automatically reprojected into this one. Default: crs of smallest Extent.
#' @param ext SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#' @param method character.
#' @param safe_output logical. If cropped data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return SpatRasters
#' @seealso
#'
#' @name crop.all
#' @export crop.all
#'
#' @examples
#'
crop.all <- function(folder_path = Input,
                     crs = NULL,
                     ext = NULL,
                     method = "smallest",
                     safe_output = FALSE,
                     ...) {

}
