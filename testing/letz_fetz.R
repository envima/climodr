library(terra)

x <- terra::rast("C:/Users/Alexander/Documents/Universität/HiWi/Daten/dem_idx.grd")
x

for (i in 1:8){
  terra::writeRaster(x[[i]], paste0("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Input/dem_idx_", i, ".tif"), overwrite = TRUE)
}

y <- terra::rast("C:/Users/Alexander/Documents/Universität/HiWi/Daten/spec_dec17.grd")
y

for (i in 1:22){
  terra::writeRaster(y[[i]], paste0("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Input/spec_dec17_", i, ".tif"), overwrite = TRUE)
}

z <- y[[1]]
z <- crop(z, c(550000, 560000, 700000, 710000))
plot(z)
terra::writeRaster(z, "C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Input/res_area.tif", overwrite = TRUE)
