library(terra)

y <- rast(file.path(envrmt$path_raster, "sch_dgm.tif"))
y <- aggregate(y, fact = 20)
plot(y)

x <- rast(file.path(envrmt$path_raster, "sch_201707.tif"))
y <- crop(y, x)
plot(y)

x <- project(x, y)
plot(x)

z <- rast(file.path(envrmt$path_dep, "res_area.tif"))
z <- aggregate(z, fact = 5)
plot(z)
z

writeRaster(x, file.path(envrmt$path_raster, "sch_201707.tif"), overwrite = TRUE)
writeRaster(y, file.path(envrmt$path_raster, "sch_dgm.tif"), overwrite = TRUE)
writeRaster(z, file.path(envrmt$path_dep, "res_area.tif"), overwrite = TRUE)
