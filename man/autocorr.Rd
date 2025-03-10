% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Autocorrelation.R
\name{autocorr}
\alias{autocorr}
\title{Autocorrelation}
\usage{
autocorr(
  envrmt = .GlobalEnv$envrmt,
  method = "monthly",
  resp,
  pred,
  plot.corrplot = TRUE,
  corrplot = "coef"
)
}
\arguments{
\item{envrmt}{variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.}

\item{method}{character. Choose the time scale your data is preserved in. Either "annual", "monthly" or "daily".}

\item{resp}{numerical. Vector or single input of the columns in the final.csv that contain your sensor data ("response variables"). The function will create one file per variable.}

\item{pred}{numerical. Vector or single input. The columns of your predictor variables, that you want to test for autocorrelation with the response variables.}

\item{plot.corrplot}{logical. Should correlation matrices be plotted?}

\item{corrplot}{character. Vector or single input. If plot.corrplot is true, you can choose the design of the correlation plot. You can choose from "coef", "crossout", "blank". Default is "coef".}
}
\value{
One .csv file per response variable. These will later be used when `autocorrelation` is set `TRUE` during `calc.model`.
}
\description{
Tests the final.csv created with `fin.csv` on autocorrelation to produce
reliable models.
}
\examples{
\dontrun{
# Test data for autocorrelation after running fin.csv
autocorr(method = "monthly",
         resp = 5,
         pred = c(8:24),
         plot.corrplot = FALSE)
}

}
\seealso{
`calc.model`
}
