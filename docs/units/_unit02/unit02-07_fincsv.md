---
title: fin.csv
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Final aggregation for CSV-Data

### Description
Extract the raster values of all raster layers from a scene at the station coordinates at each time stamp. 
The extracted data will be attached to the station data so there is a .csv-file with coordinates, sensor data (response values) 
and extracted raster data (predictor values). The data is ready to be used for modelling.

### Usage
```r
fin.csv(method = "monthly", crs = NULL, save_output = TRUE, ...)
```

### Arguments
**method**		character. Either "daily", monthly" or "annual". Also depends on the available data.  
**save_output**	logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE. Otherwise the output will be saved in the temporary directory. Default: FALSE.  

### Value
Data Frame

### See also
`prep.csv`, `proc.csv`, `spat.csv`, `calc.indices`

## Examples

```r
## Not run: 
csv_fin <- fin.csv(method = "monthly",
                   save_output = TRUE)
head(csv_fin)

## End(Not run)
```
