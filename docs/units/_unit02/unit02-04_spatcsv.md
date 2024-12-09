---
title: spat.csv
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Spatial aggregation for CSV-Data

### Description
Extract station coordinates from meta-data and reproject the coordinates to the project coordinate reference system.

### Usage
```r
spat.csv(method = "monthly", des_file, crs = NULL, save_output = TRUE, ...)
```

### Arguments
**method**		character. Either "daily", monthly" or "annual". Also depends on the available data.  
**des_file**	character. The filename and data type of the meta-data. (Only reads .csv)  
**crs**			character. EPSG of the Coordinate Reference System, if no **res_area.tif** file is provided.  
**save_output**	logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE. Otherwise the output will be saved in the temporary directory. Default: TRUE  

### Value
Data Frame

### See also
`prep.csv`, `proc.csv`, `fin.csv`

## Examples
```r
## Not run: 
csv_spat <- spat.csv(method = "monthly",
                     des_file = "plot_description.csv",
                     save_output = TRUE)
head(csv_spat)

## End(Not run)
```
