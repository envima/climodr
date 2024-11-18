## code to prepare `Vignette` dataset goes here
climodr::envi.create("E:/climodr/vignette")
library(usethis)
library(terra)

# Input vector folder
ext_vignette <- vect(file.path(envrmt$path_vector, "ext_vignette.gpkg"))
crs(ext_vignette) <- crs(stringi::stri_enc_toascii(crs(ext_vignette)))
ext_vignette <- wrap(ext_vignette)
usethis::use_data(ext_vignette, overwrite = TRUE)
rm(ext_vignette)

# Input tabular folder
x <- list.files(envrmt$path_tabular)
for (i in 1:length(x)){
  n <- gsub(x = x[i], pattern = ".csv$", replacement = "")
  assign(n, read.csv(file.path(envrmt$path_tabular, x[i])))
  eval(call("use_data", as.name(n), overwrite = TRUE))
}
rm(list = setdiff(ls(), "envrmt"))

# Input raster folder
x <- list.files(envrmt$path_raster)
for (i in 1:length(x)){
  n <- gsub(x = x[i], pattern = ".tif$", replacement = "")
  assign(n, wrap(rast(file.path(envrmt$path_raster, x[i]))))
  eval(call("use_data", as.name(n), overwrite = TRUE))
}
rm(list = setdiff(ls(), "envrmt"))

# Input dep folder
plot_description <- read.csv(file.path(envrmt$path_dep, "plot_description.csv"))
res_area <- wrap(rast(file.path(envrmt$path_dep, "res_area.tif")))
usethis::use_data(plot_description, overwrite = TRUE)
usethis::use_data(res_area, overwrite = TRUE)
rm(list = setdiff(ls(), "envrmt"))
