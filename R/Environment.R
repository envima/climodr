#' Create climodr environment
#'
#' Creates an environment climodr will use during the calculation process. A list is returned with all paths to all folders.
#' After creating the environment, all necessary data should be stored into the depending Input sub-folders.
#' There is also an additional temp-folder, where temporary data is stored, which can be deleted after not being used anymore.
#'
#'
#' @param proj_path character. Path to project directory. Climodr will work exclusively in this folder and create all project folders in here.
#' @param memfrac numeric. Value between 0 and 0.9. The fraction of RAM that may be used by the terra package
#'
#' @return list. Contains all paths to each folder in the project directory. Necessary for climodr to operate its functions.
#'
#' @name envi.create
#' @export envi.create
#'
#' @examples
#' \dontrun{
#' # create climodr environment and allow terra-functions to use 70% of RAM
#' envi.create("C:/user/climodr_user/project_directory",
#'              memfrac = 0.7)
#' }
#'
envi.create <- function(proj_path,
                        memfrac = NULL,
                        ...) {
  projectDirList = c("input/dep/",
                     "input/raster/",
                     "input/tabular/",
                     "input/vector/",
                     "output/predictions/",
                     "output/predictions/aoa/",
                     "output/maps/",
                     "output/rfinal/",
                     "output/statistics/",
                     "output/tfinal/",
                     "workflow/models/",
                     "workflow/tworkflow/",
                     "workflow/tmp/",
                     "workflow/rworkflow/",
                     "workflow/vworkflow/"
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

  if(is.null(memfrac)){
    terra::terraOptions(tempdir = envrmt$path_tmp)
  } else {
    terra::terraOptions(tempdir = envrmt$path_tmp,
                        memfrac = memfrac)
  }
  print(
    paste0(
      "Succesfully created an environment in ",
      proj_path,
      "."
      )
    )
}

#' Load in Example Data
#'
#' Climodr comes with a full set of example data. But since this package runs primarily with data,
#' that is not linked to the global environment, but saved in local folders build via `envi.create`,
#' one can't just load example data. This function will load all the example data used in the vignette
#' into your climodr environment. This way you can run all the code from the vignette.
#'
#' @return Multiple files used by the climodr vignette
#'
#' @name clim.sample
#' @export clim.sample
#'
#' @examples
#' \dontrun{
#' # Load the climodr example data into the current climodr environment
#' clim.sample(overwrite = TRUE)
#' }
#'
clim.sample <- function(overwrite){
  print("Loading example data for the climodr example..")
# Input dep folder
  data("res_area")
  data("plot_description")
  res_area <- terra::unwrap(res_area)

  terra::writeRaster(res_area, file.path(envrmt$path_dep, "res_area.tif"), overwrite = TRUE)
  write.csv(plot_description, file.path(envrmt$path_dep, "plot_description.csv"))

  print(paste0("Saved climodr example dependency files to {", envrmt$path_dep, "}."))
  rm(list = setdiff(ls(), "envrmt"))
  gc()

# Input raster folder
  data("sch_201707")
  data("sch_dgm")
  sch_201707 <- terra::unwrap(sch_201707)
  sch_dgm <- terra::unwrap(sch_dgm)

  terra::writeRaster(sch_201707, file.path(envrmt$path_raster, "sch_201707.tif"), overwrite = TRUE)
  terra::writeRaster(sch_dgm, file.path(envrmt$path_raster, "sch_dgm.tif"), overwrite = TRUE)

  print(paste0("Saved climodr example raster files to {", envrmt$path_raster, "}."))
  rm(list = setdiff(ls(), "envrmt"))
  gc()

# Input tabular folder
  l <- data(package = "climodr")$results[,3]
  l <- l[grepl("SE", l)]

  for (i in l){
    eval(call("data", i))
    eval(call("write.csv", as.name(i), file.path(envrmt$path_tabular, paste0(i, ".csv")), row.names = FALSE))
  }

  print(paste0("Saved climodr example tabular files to {", envrmt$path_tabular, "}."))
  rm(list = setdiff(ls(), "envrmt"))
  gc()

# Input vector folder
  data("ext_vignette")
  ext_vignette <- terra::unwrap(ext_vignette)

  terra::writeVector(ext_vignette, file.path(envrmt$path_vector, "ext_vignette.gpkg"), overwrite = TRUE)

  print(paste0("Saved climodr example vector files to {", envrmt$path_vector, "}."))
  rm(list = setdiff(ls(), "envrmt"))
  gc()

# Talk to the User
  print("Done loading all the example files. You are ready to continue.")
}

