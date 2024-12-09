---
title: calc.indices
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Calculate spectral indices

### Description
Calculates a set of spectral indices to have more predictor variables available when further modeling.

### Usage
```r
calc.indices(
  vi = "all",
  bands = c("blue", "green", "red", "nir", "nirb", "re1", "re2", "re3", "swir1", "swir2"),
  overwrite = FALSE
)
```

### Arguments
**vi**			Character. Either "all" or vector containing the preferred spectral indices. See 'Details' for more information.  
**bands**		Character. Vector with lenght(bands) = 10. Contains the names of the bands in the Raster Stack. If bands from the *Usage* example vector dont exist, use "NA" in their position. See 'Details' for more information.  
**overwrite**	logical. Argument passed down from 'terra'-package. Overwrite existing files?  

### Value
SpatRaster-Stack

### See also
`crop.all`, `fin.csv`

## Examples

```r
## Not run: 
calc.indices(vi = "all",
             bands = c("blue", "green", "red",
                       "nir", "nirb",
                       "re1", "re2", "re3",
                       "swir1", "swir2"),
             overwrite = TRUE)

## End(Not run)
```
