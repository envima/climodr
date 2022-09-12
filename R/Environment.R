#' Link input and output folders for climodr
#'
#' For easier and automated use of climodr. This function links either the
#' folder where all necessary input data is stored and also the folder,
#' where all relevant output data will be saved. Also generates information,
#' what data is stored in Input.
#'
#' @param in_dir character. Path to input folder. All necessary and relevant data should be stored here.
#' @param out_dir character. Path to output folder. Save all products in this destination.
#'
#' @return NULL
#' @seealso envi.create
#'
#' @name hubs
#' @export hubs
#'
#' @examples
#' \dontrun{
#' hubs("C:/Users/R_User/cool_proj/data/Input",
#'      "C:/Users/R_user/cool_proj/data/Output")
#' }
#'
hubs <- function(in_dir,out_dir) {
    Input <- in_dir;
    Output <- out_dir;
    all_files_in_distribution <- list.files(path = in_dir, recursive = T); #reads all data in Input-Folder
    tiff_paths <- grep(".tif$", all_files_in_distribution, value=TRUE); # Select tiff-files
    number_of_tiffs <- length(tiff_paths);
    csv_paths <- grep(".tif$", all_files_in_distribution, value=TRUE);
    number_of_csvs <- length(csv_paths)
}


#' Create temporary environment
#'
#' Creates an environment climodr will use to store necessary, temporary data, which will be deleted after not being used anymore.
#'
#' @param proj_path character. Path for working directory.
#'
#' @return NULL
#' @seealso [climodr::hubs] which climodr requires to be set up completely
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
envi.create <- function(proj_path) {
  if (!exists("rootDIR")) {
    cat("variable rootDIR is NOT defined\n A temporary directory is set by default", "!\n")
    rootDIR = tempdir()
  };
  if (!exists("alt_env_id")) {
    cat("variable alt_env_id is NOT defined\n 'COMPUTERNAME' is set by default", "!\n")
    alt_env_id = "COMPUTERNAME"
  };
  if (!exists("alt_env_value")) {
    cat("variable alt_env_value is NOT defined\n 'PCRZP' is set by default", "!\n")
    alt_env_value = "PCRZP"
  };
  if (!exists("alt_env_root_folder")) {
    cat("variable alt_env_root_folder is NOT defined\n Input is set by default", "!\n")
    alt_env_root_folder = proj_path
  };

  rootDir = envimaR::alternativeEnvi(root_folder = rootDIR,
                                     alt_env_id = alt_env_id,
                                     alt_env_value = alt_env_value,
                                     alt_env_root_folder = alt_env_root_folder);

  projectDirList = c("preds/",                # folder for the predictor variables
                     "raster/",               # folder for raster data
                     "raster/mean_specdec17idx/",  # sub folder for the meaned bale_dec_17 spectral idx
                     "raster/dec17/",     # sub folder for the bale_raster data
                     "raster/geodata/soil/bedrock/", # sub folder for geodata - bedrock layer
                     "raster/geodata/soil/texture/", # sub folder for geodata - texture layer
                     "vector/",               # folder for vector data
                     "tmp/",                  # folder for temporary stuff
                     "ffsmodels/",            # folder for the ffsmodels
                     "ffspredictions/",       # folder for the ffspredictions
                     "varImp_model/"          # folder for the varImp_models
                     );

  if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") {
    projectDirList = append(projectDirList,appendProjectDirList)
  };

  if (exists("appendpackagesToLoad") && appendpackagesToLoad[[1]] != "") {
    packagesToLoad = append(packagesToLoad,appendpackagesToLoad)
  };

  envrmt = envimaR::createEnvi(root_folder = rootDir,
                               folders = projectDirList,
                               path_prefix = "path_",
                               alt_env_id = "COMPUTERNAME",
                               alt_env_value = "PCRZP",
                               alt_env_root_folder = proj_path);

  raster::rasterOptions(tmpdir = envrmt$path_tmp);
  rgdal::set_thin_PROJ6_warnings(TRUE);
}
