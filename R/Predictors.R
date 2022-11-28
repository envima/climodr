#' Calculate DEM-Based Predictors
#'
#' blabla
#'
#' @param
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return geotiff
#' @seealso
#'
#' @name elev.idx
#' @export elev.idx
#'
#' @examples
#'
elev.idx <- function(proj = NULL,
                     path = NULL){
  data <- read.csv(file.path(envrmt$path_tmp, paste0("csv_spat_mm.csv")));
  c_st=shapefile(file.path(envrmt$path_vector, "project_climate_stations.shp"));


}

#' Calculate Spectral-Based Predictors
#'
#' blabla
#'
#' @param
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return geotiff
#' @seealso
#'
#' @name spec.idx
#' @export spec.idx
#'
#' @examples
#'
spec.idx <- function(){

}

#' Bind Predictors
#'
#' blabla
#'
#' @param
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return geotiff
#' @seealso
#'
#' @name
#' @export
#'
#' @examples
#'
