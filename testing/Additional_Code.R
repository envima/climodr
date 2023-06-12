# Preselect data

climodr::envi.create("E:/climodr/showcase")

csv_list <- list()
Input <- envrmt$path_tabular

all_files_in_distribution <- list.files(path = Input, recursive = T)

csv_paths <- grep(".csv$", all_files_in_distribution, value=TRUE)
number_of_csvs <- length(csv_paths)

for (i in 1:number_of_csvs){
  csv_data <- read.csv(file.path(Input, paste0(csv_paths[[i]])), sep = ",")
  cn_data <- colnames(csv_data)
  number_of_cn <- length(cn_data)
  csv_data <- csv_data[,c(1:3,6,9:10)]
  write.csv(csv_data, file.path("E:/climodr/vignette/input/tabular", paste0("2018_", csv_data[[1,1]], ".csv")), row.names = FALSE)
}

# preselect raster

climodr::envi.create("E:/climodr/vignette")

x <- terra::rast(file.path(envrmt$path_wraster, "2018_vignette_layer.tif"))
n <- names(x)
nn <- n[-c(14, 17, 19, 20)]
xn <- terra::subset(x, nn)

terra::writeRaster(xn,
                   file.path(envrmt$path_wraster, "2018_vignette_layer_2.tif"),
                   overwrite = TRUE)

d <- terra::rast(file.path(envrmt$path_wraster, "dem_vignette.tif"))
n <- names(d)
nn <- n[-c(4, 5)]
dn <- terra::subset(d, nn)

terra::writeRaster(dn,
                   file.path(envrmt$path_wraster, "dem_vignette_2.tif"),
                   overwrite = TRUE)
