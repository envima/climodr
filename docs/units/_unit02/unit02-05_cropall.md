---
title: crop.all
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Cropping tiff data

### Description
Crops input data to the extent size and reprojects them into project Coordinate reference system.

### Usage
```r
crop.all(
  method = "MB_Timeseries",
  crs = NULL,
  ext = NULL,
  overwrite = FALSE,
  ...
)
```

### Arguments
**method**	character. Use "MB_Timeseries" for now. More methods are planned and will be added in future.  
**crs**		Coordinate reference system Used to crop all images in folder_path. If crs it will automatically reprojected into this one. Default: crs of smallest Extent.  
**ext**		SpatRaster, SpatVector or SpatExtent. Extent all data is cropped into. Default: Smallest Extent in folder_path.  

### Value
SpatRaster-Stack. Also saved to "/workflow/rworkflow"

### See also
`fin.csv`, `calc.indices`

## Examples
```r
## Not run: 
crop.all(method = "MB_Timeseries",
         overwrite = TRUE)

## End(Not run)
```
