devtools::load_all()

### Initiate Climodr ###
hubs("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Input/",
     "C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Output/")

envi.create("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/")

### Pre-Processing ###

# Cropping

crop.all(method = "Input", safe_output = TRUE)

x <- terra::rast(paste0(Output, "tiff_stack.tif"))
x
plot(x)
