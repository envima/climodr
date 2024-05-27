devtools::load_all()

### Initiate climodr ###
climodr::envi.create("E:/climodr/vignette")

### Pre-Processing ###

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

### Modelling ###

#---- Calculate Models ----#

# check data for autocorrelation
# vector with excluded data
eval_vector <- c(5, 6, 9:47)
climodr::autocorr(eval_vector)

# Modelling
climodr::calc.model(timespan = c(18),
                    climresp = c(6:9),
                    classifier = c("rf", "pls","nnet" ,"lm"),
                    seed = 707,
                    p = 0.8,
                    folds = "all",
                    mnote = "testrn",
                    predrows = c(16:55),
                    tc_method = "cv",
                    metric = "RMSE",
                    doParallel = TRUE,
                    autocorrelation = TRUE)

# Prediction (inclusive AOA)
climodr::climpred(doParallel = TRUE,
                  AOA = TRUE)
