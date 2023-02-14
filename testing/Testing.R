devtools::load_all()

### Initiate Climodr ###
climodr::envi.create("D:/users/kluga/showcase")

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

# Modelling
climodr::calc.model(timespan = c(18),
                    climresp = c(6), # c(6, 9, 12, 13)
                    classifier = c("pls","lm"),
                    seed = 707,
                    p = 0.8,
                    fold = "LLO",
                    mnote = "no0410",
                    predrows = c(16:55),
                    tc_method = "cv",
                    metric = "RMSE",
                    doParallel = TRUE
)

# Prediction
climodr::climpred(fold = "LLO",
                  AOA = FALSE)
