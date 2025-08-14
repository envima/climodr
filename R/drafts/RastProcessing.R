#' Prepare Raster Data
#'
#' Crops input data to the extent size and reprojects them into project Coordinate reference system.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param crs    Coordinate reference system Used to crop all images in folder_path.
#' @param ext    SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.
#' @param overwrite logical. Should existing files with the same filename be
#'               overwritten? Default = FALSE
#' @param ...    arguments passed down from other functions.
#'
#' @return SpatRaster-Stack. Also saved to /workflow/rworkflow
#' @seealso `fin.csv`, `calc.indices`
#'
#' @name prepRasterData
#' @export prepRasterData
#'
#' @examples
#' #create climodr environment and allow terra-functions to use 70% of RAM
#'
#'

prepRasterData <- function(envrmt = .GlobalEnv$envrmt,
                           datepos,
                           dateformat,
                           crs = NULL,
                           ext = NULL){
  date
}
