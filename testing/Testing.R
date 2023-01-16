devtools::load_all()

### Initiate Climodr ###
climodr::envi.create("E:/climodr/showcase")

### Pre-Processing ###

# Cropping tiffs

# x <- climodr::crop.all(method = "Input", safe_output = TRUE)

x <- terra::rast(file.path(envrmt$path_wraster, "2018_testdata_layer.tif"))
x
terra::plot(x[[1:4]])

#---- Aggregate CSVs ----#

# Prepare CSV-Data
climodr::prep.csv(method = "proc", safe_output = TRUE)

# Check the created csv files
csv_files <- grep("_no_NAs.csv$", list.files(envrmt$path_tworkflow), value=TRUE)
csv_files

# Process CSV-Data
csv_data <- climodr::proc.csv(method = "all",
                              rbind = TRUE,
                              safe_output = TRUE)
# Check again
head(csv_data)

# Create spatial CSV-Data
csv_spat <- climodr::spat.csv(method = "monthly",
                              des_file = "plot_description.csv",
                              safe_output = TRUE)
# Check again
head(csv_spat)

# Finalize CSV-Data
csv_fin <- climodr::fin.csv(method = "monthly",
                            safe_output = TRUE)
# Check one last time
head(csv_fin)

# Modelling
climodr::calc.model(timespan = c(18),
                    response = c(6,9,12,13),
                    classifier = c("rf", "pls","nnet" ,"lm"),
                    seed = 707,
                    p = 0.8,
                    fold = "LLO",
                    mnote = "normal",
                    predrows = c(16:56),
                    tc_method = "cv",
                    metric = "RMSE")
