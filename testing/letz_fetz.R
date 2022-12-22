#clean environment
rm(list=ls())
#session restart
.rs.restartR()
#remove plots_tubeDB
dev.off()

# 0 - specific setup
#-----------------------------
require(envimaR)

# MANDANTORY: defining the root folder, DO NOT change this line:
rootDIR = "C:/Users/Alexander/Documents/Uni/HiWi/Test/data/"

#-- Further customization of the setup by the user this section
#-- can be freely customized only the definition of additional packages
#-- and directory paths MUST be done using the two variables
#-- appendpackagesToLoad and appendProjectDirList
#-- feel free to remove this lines if you do not need them
# define  additional packages uncomment if necessary
# appendpackagesToLoad = c("dummy-package")
# define additional subfolders uncomment if necessary
# appendProjectDirList =  c("data/dymmy-folder/")

devtools::load_all()

climodr::hubs(paste0(rootDIR, "Input/"),
              paste0(rootDIR, "Output/")
)

Bale_tubeDB <- read.csv(paste0(Input, "plots_tubeDB.csv"))

v <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010")
for (i in v){
  x <- data.frame(Bale_tubeDB[c(which(Bale_tubeDB$plot == paste0("BALE", i))[[1]]:tail(which(Bale_tubeDB$plot == paste0("BALE", i)), n = 1)), ])
  write.csv(x, paste0(Output, "BALE", i, ".csv"), row.names = FALSE)
}



csv_list <- list()
Input <- envrmt$path_tabular

all_files_in_distribution <- list.files(path = Input, recursive = T) #reads all data in Input-Folder

csv_paths <- grep(".csv$", all_files_in_distribution, value=TRUE)
number_of_csvs <- length(csv_paths)

for (i in 1:number_of_csvs){
  csv_data <- read.csv(file.path(Input, paste0(csv_paths[[i]])), sep = ",")
  csv_new <- csv_data[which(csv_data$year == 18), ]
  csv_new$year <- NULL
  csv_new$X <- NULL
  write.csv(csv_new, file.path(envrmt$path_tabular, paste0(csv_paths[[i]])), row.names = FALSE)
}
