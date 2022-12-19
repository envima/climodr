#' Create climodr environment
#'
#' Creates an environment climodr will use to store necessary, temporary data, which will be deleted after not being used anymore.
#'
#' @param proj_path character. Path for working directory.
#'
#' @return NULL
#' @seealso [envimaR::alternativeEnvi], [envimaR::createEnvi] which this function wraps.
#'
#' @name envi.create
#' @export envi.create
#'
#' @examples
#' \dontrun{
#' envi.create("C:/USERS/R_User/cool_proj")
#' }
#'
envi.create <- function(proj_path,
                        alt_env = NULL,
                        ...) {
  projectDirList = c("input/dep/",
                     "input/raster/",
                     "input/tabular/",
                     "input/vector/",
                     "output/predictions/",
                     "output/maps/",
                     "output/statistics/",
                     "workflow/models/",
                     "workflow/preds/",
                     "workflow/raster/rworkflow/",
                     "workflow/raster/rfinal/",
                     "workflow/tabular/tworkflow/",
                     "workflow/tabular/tfinal/",
                     "workflow/tmp/",
                     "workflow/vector/vworkflow/",
                     "workflow/vector/final/"
                     );

  if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") {
    projectDirList = append(projectDirList,appendProjectDirList)
  };

  if (is.null(alt_env)){
    alt_env <- tempdir()
  }

  envrmt <<- envimaR::createEnvi(root_folder = proj_path,
                                 folders = projectDirList,
                                 path_prefix = "path_",
                                 alt_env_id = "COMPUTERNAME",
                                 alt_env_value = "PCRZP",
                                 alt_env_root_folder = alt_env);

  raster::rasterOptions(tmpdir = envrmt$path_tmp);
  rgdal::set_thin_PROJ6_warnings(TRUE);
}
