#' Link Folders for Package
#'
#' For easier and automatet use, this function links either the folder where all
#' necessary Input Data is stored and also the folder, where all relevant
#' output is saved.
#' @param input_directory_path
#' @param output_directory_path
#' @examples
#' hubs("C:/Users/R_User/cool_proj/data/Input",
#'      "C:/Users/R_user/cool_proj/data/Output")
#' @export

hubs <- function(input_directory_path, output_directory_path) {
  Input <- input_directory_path;
  Output <- output_directory_path
}

#' Create temporary Environment
#'
#' Creates an environment the package will use to store necessary, temporary
#' data
#' @param proj_path path for temporary environment
#' @examples
#' envi.create("C:/Useres/R_User/cool_proj")
#' @export

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
  print("done")
}
