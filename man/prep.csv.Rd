% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PreProcessing.R
\name{prep.csv}
\alias{prep.csv}
\title{Preparing CSV-Data}
\usage{
prep.csv(envrmt = .GlobalEnv$envrmt, method = "proc", save_output = TRUE, ...)
}
\arguments{
\item{envrmt}{variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.}

\item{method}{character. "proc" for ready-to-use data in separate .csv-files. "tube" for raw-data from the Tube Data Base. Default "proc"-Method.}

\item{save_output}{logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE.
Otherwise the output will be saved in the temporary directory. Default: FALSE.}

\item{...}{arguments passed down from other functions.}
}
\value{
List
}
\description{
Crops input data to the extent size and removes NA-Values
}
\examples{
#create climodr environment and allow terra-functions to use 70\% of RAM
envrmt <- envi.create(proj_path = tempdir(),
                      memfrac = 0.7)

# Load the climodr example data into the current climodr environment
clim.sample(envrmt = envrmt)

#prepare csv-files
prep.csv(envrmt = envrmt,
         method = "proc",
         save_output = TRUE)

#check the created csv files
csv_files <- grep("_no_NAs.csv$",
                  list.files(envrmt$path_tworkflow),
                  value=TRUE)
csv_files

}
\seealso{
`proc.csv`, `spat.csv`, `fin.csv`
}
