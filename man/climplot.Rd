% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plotting.R
\name{climplot}
\alias{climplot}
\title{Create Maps using the `terra` package graphic parameters}
\usage{
climplot(
  envrmt = .GlobalEnv$envrmt,
  mnote,
  sensor,
  aoa = FALSE,
  mapcolors = rev(grDevices::terrain.colors(50)),
  scale_position = "bottomleft",
  north_position = "topright"
)
}
\arguments{
\item{envrmt}{variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.}

\item{mnote}{character. The modelnote you want to create maps of.}

\item{sensor}{character. The sensor you want to create maps for.}

\item{aoa}{logical. Do you want the area of applicability to be added to your map?}

\item{mapcolors}{The color pallete you want to use for the map. Default is `rev(grDevices::terrain.colors(50))`}

\item{scale_position}{character. Graphical parameter. The relative positiion of the Scale for the map. See `terra::plot` for more details.}

\item{north_position}{character. Graphical parameter. The relative positiion of the Scale for the map. See `terra::plot` for more details.}
}
\value{
Maps in PNG-Format to your harddrive.
}
\description{
Plot results of climodr into maps. Right now maps are created using the terra package.
The maps created are very basic. Will be updated to run with tidyterra in future.
}
\examples{
\dontrun{
# Create a Temperature Map from the vignette model
climplot(mnote = "vignette",
         sensor = "Ta_200",
         aoa = FALSE,
         mapcolors = rev(heat.colors(50)),
         scale_position = "bottomleft",
         north_position = "topright")
}
}
\seealso{
`terra::plot`
}
