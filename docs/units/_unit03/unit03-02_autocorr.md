---
title: autocorr
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Test for Autocorrelation

### Description
Tests the final.csv created with 'fin.csv' on autocorrelation to produce reliable models.

### Usage
```r
autocorr(
  method = "monthly",
  resp,
  pred,
  plot.corrplot = TRUE,
  corrplot = "Coef"
)
```

### Arguments
**method** 			character. Choose the time scale your data is preserved in. Either "annual", "monthly" or "daily".  
**resp** 			numerical. Vector or single input of the columns in the final.csv that contain your sensor data ("response variables"). The function will create one file per variable.  
**pred** 			numerical. Vector or single input. The columns of your predictor variables, that you want to test for autocorrelation with the response variables.  
**plot.corrplot** 	logical. Should correlation matrices be plotted?  
**corrplot** 		character. Vector or single input. If plot.corrplot is true, you can choose the design of the correlation plot. You can choose from "coef", "crossout", "blank". Default is "coef".  

### Value
One .csv file per response variable. These will later be used when 'autocorrelation' is set 'TRUE' during 'calc.model'.

### See Also
`calc.model`

## Examples
```r
## Not run: 
# Test data for autocorrelation after running fin.csv
autocorr(method = "monthly",
         resp = 5,
         pred = c(8:24),
         plot.corrplot = FALSE)

## End(Not run)
```
