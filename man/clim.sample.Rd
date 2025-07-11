% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Environment.R
\name{clim.sample}
\alias{clim.sample}
\title{Load in Example Data}
\usage{
clim.sample(envrmt = .GlobalEnv$envrmt)
}
\arguments{
\item{envrmt}{variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.}
}
\value{
Multiple files used by the climodr vignette
}
\description{
Climodr comes with a full set of example data. But since this package runs primarily with data,
that is not linked to the global environment, but saved in local folders build via `envi.create`,
one can't just load example data. This function will load all the example data used in the vignette
into your climodr environment. This way you can run all the code from the vignette.
}
\examples{
#create climodr environment and allow terra-functions to use 70\% of RAM
envrmt <- envi.create(proj_path = tempdir(),
                      memfrac = 0.7)

# Load the climodr example data into the current climodr environment
clim.sample(envrmt = envrmt)

}
