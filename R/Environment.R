#' Create climodr environment
#'
#' Creates an environment climodr will use to store necessary, temporary data, which will be deleted after not being used anymore.
#'
#' @param proj_path character. Path for working directory.
#'
#' @return NULL
#' @seealso
#'
#' @name envi.create
#' @export envi.create
#'
#' @examples
#' \dontrun{
#'
#' }
#'
envi.create <- function(proj_path,
                        ...) {
  projectDirList = c("input/dep/",
                     "input/raster/",
                     "input/tabular/",
                     "input/vector/",
                     "output/predictions/",
                     "output/predictions/aoa/",
                     "output/maps/",
                     "output/statistics/",
                     "workflow/models/",
                     "workflow/preds/",
                     "workflow/tabular/tworkflow/",
                     "workflow/tabular/tfinal/",
                     "workflow/tmp/",
                     "workflow/wraster/",
                     "workflow/wvector/"
                     );

  if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") {
    projectDirList = append(projectDirList,appendProjectDirList)
  };

  createFolders <- function(root_folder,
                            folders,
                            folder_names = NULL,
                            path_prefix = "path_",
                            create_folders = TRUE
                            ){
    folders <- lapply(folders, function(f) {
      file.path(root_folder, f)
    })
    folders <- folders[!duplicated(folders)]

    if (is.null(folder_names)) {
      names(folders) <- basename(unlist(folders))
      tmplt <- unlist(folders)

      while (any(duplicated(names(folders)))) {
        tmplt <- dirname(tmplt)
        dplcts <- which(duplicated(names(folders), fromLast = FALSE) |
                          duplicated(names(folders), fromLast = TRUE))
        names(folders)[dplcts] <-
          paste(basename(tmplt)[dplcts], names(folders[dplcts]), sep = "_")
      }
    } else {
      names(folders) <- folder_names
    }

    if (!is.null(path_prefix)) names(folders) <- paste0(path_prefix, names(folders))

    # Check paths for existance and create if necessary
    for (f in folders) {
      if (!file.exists(f)) dir.create(f, recursive = TRUE)
    }

    return(folders)
  }

  createEnv <- function(path = tempdir(),
                        folders = c("data", "data/tmp"),
                        folder_names = NULL,
                        path_prefix = NULL,
                        create_folders = TRUE
                        ){
    folders <- createFolders(path,
                             folders,
                             folder_names = folder_names,
                             path_prefix = path_prefix,
                             create_folders = NULL
    )

    return(folders)
  }

  envrmt <<- createEnv(path = proj_path,
                       folders = projectDirList,
                       path_prefix = "path_")

  raster::rasterOptions(tmpdir = envrmt$path_tmp);
  rgdal::set_thin_PROJ6_warnings(TRUE);
}

