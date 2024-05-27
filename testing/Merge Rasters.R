tiff_paths <- list.files(envrmt$path_raster, full.names = TRUE)

# 28 - 36
dates <- c()

for (i in 1:length(tiff_paths)){
  dates[i] <- str_sub(tiff_paths[i], 78, 85)
}
dates <- unique(dates)

for (i in 1:length(dates)){
  print(paste0("Running ", dates[i], ". ", i, "/", length(dates)))
  paths <- tiff_paths[which(str_sub(tiff_paths, 78, 85) == dates[i])]
  a <- terra::rast(paths[1])
  b <- terra::rast(paths[2])
  c <- terra::rast(paths[3])
  d <- terra::rast(paths[4])

  total <- terra::merge(a, b, c, d)
  terra::writeRaster(total, file.path(envrmt$path_raster, paste0("sch_", dates[i], ".tif")))

  remove(paths, a, b, c, d, total)
  gc()
  print(paste0("Finished ", dates[i], "."))
}

# mean rasters (short)
a <- terra::rast(tiff_paths[1])
b <- terra::rast(tiff_paths[2])
c <- terra::mosaic(a, b, fun = "mean")
terra::writeRaster(c, file.path(envrmt$path_raster, paste0("sch_201707_mean.tif")))

# mean rasters
all_files <- list.files(path = file.path(envrmt$path_rfinal), recursive = T); #reads all data in Workflow Raster Folder
dates <- as.Date(stringr::str_sub(all_files, 5, 12), format = "%Y%m%d")
months <- strftime(dates, format = "%m")

for (i in unique(months)[10:12]){
  print(paste0("Calculating means for ",
               unique(strftime(dates, format = "%B"))[as.integer(i)],
               "..  ",
               i, "/", length(unique(months))))
  month_rasters <- all_files[which(months == i)]
  rstack <- terra::rast()
  rmean <- terra::rast()

  print(paste0("Reading ", length(month_rasters), " files for this month..  "))

  for (j in 1:length(month_rasters)){
    x <- terra::rast(file.path(envrmt$path_rfinal, month_rasters[j]))
    terra::add(rstack) <- x
  } # end j-loop

  n <- terra::nlyr(rstack)
  l <- length(terra::sources(rstack))
  vsort <- names(rstack)[1:(n/l)]
  rstack <- rstack[[sort(names(rstack))]]

  print("Calculate monthly means for each layer.. ")
  for (k in seq(1, n, l)){
    print("  ...  ")
    a <- rstack[[k:(k+l-1)]]
    b <- terra::app(a, fun = "mean")
    names(b) <- names(a)[1]
    terra::add(rmean) <- b
  } # end k-loop

  print("Writing raster..")
  terra::writeRaster(rmean,
                     file.path(envrmt$path_rfinal,
                               paste0("hai_2020",
                                      unique(months)[as.integer(i)],
                                      "_mean.tif")
                     )
  )
  remove(rstack, rmean, j, n, l, vsort, k, a, b)
  gc()
} # end i-loop
