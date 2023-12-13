# 28 - 36
dates <- c()

for (i in 1:length(tiff_paths)){
  dates[i] <- str_sub(tiff_paths[i], 54, 61)
}
dates <- unique(dates)

for (i in 1:length(dates)){
  print(paste0("Running ", dates[i], ". ", i, "/", length(dates)))
  paths <- tiff_paths[which(str_sub(tiff_paths, 54, 61) == dates[i])]
  a <- terra::rast(file.path(envrmt$path_raster, paths[1]))
  b <- terra::rast(file.path(envrmt$path_raster, paths[2]))
  c <- terra::rast(file.path(envrmt$path_raster, paths[3]))
  d <- terra::rast(file.path(envrmt$path_raster, paths[4]))

  total <- terra::merge(a, b, c, d)
  terra::writeRaster(total, file.path(envrmt$path_raster, paste0("hai_", dates[i], ".tif")))

  remove(paths, a, b, c, d, total)
  gc()
  print(paste0("Finished ", dates[i], "."))
}

dgm <- terra::rast(file.path(envrmt$path_tmp, "be_hai_dgm5_____.tiff"))
terra::writeRaster(dgm, file.path(envrmt$path_raster, "hai_dgm.tif"))
remove(dgm)
