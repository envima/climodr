devtools::load_all()

### Initiate Climodr ###
climodr::hubs("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Input/",
     "C:/Users/Alexander/Documents/Universität/HiWi/Test/data/Output/")

climodr::envi.create("C:/Users/Alexander/Documents/Universität/HiWi/Test/data/")

### Pre-Processing ###

# Cropping

climodr::crop.all(method = "Input", safe_output = TRUE)

x <- terra::rast(paste0(Output, "tiff_stack.tif"))
x
terra::plot(x)

