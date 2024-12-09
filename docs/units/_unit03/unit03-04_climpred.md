---
title: climpred
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Predict sensor data area wide

### Description
Use the models created using 'calc.model' to predict the modeled data onto a full spatial raster scene.

### Usage
```r
climpred(method = "monthly", mnote, AOA = TRUE)
```

### Arguments
**method** 			Character. Either "daily", monthly" or "annual". Also depends on the available data.
**mnote** 			Character. Model note to filter models for the fitting model run.  
**AOA** 			Logical. Should the Area of Applicability be calculated additional to the models?  

### Value
SpatRaster. Multiple predictions stored in the "/output/predictions/" folder.

### See Also
`autocorr`, `predict`

## Examples
```r
## Not run: 
climpred(method = "monthly",
         mnote = "normal",
         AOA = TRUE)
predlist <- list.files(envrmt$path_predictions,
                       pattern = ".tif")
head(predlist)

## End(Not run)
```
