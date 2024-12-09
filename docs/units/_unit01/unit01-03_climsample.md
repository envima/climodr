---
title: clim.sample
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Load example data

### Description
Climodr comes with a full set of example data. But since this package runs primarily with data, that is not linked to the global environment, 
but saved in local folders build via 'envi.create', one can't just load example data. This function will load all the example data used in the 
vignette into your climodr environment. This way you can run all the code from the vignette.

### Usage
```r
clim.sample()
```

### Value
Multiple files used by the climodr vignette

## Examples
```r
## Not run: 
# Load the climodr example data into the current climodr environment
clim.sample()

## End(Not run)
```

## Structure

In total 15 files will be loaded into your climodr environment.  

| Name |  Input-Location | Type                        | Content                                                                          |
|---------|-------|------------------------------|----------------------------------------------------------------------------------|
| ext_vignette | vector | SpatVector     | polygon with the areo of interest for precise masking |
| plot_description | dep | dataframe                   | imaginary coordinates of the example stations |
| res_area | dep | SpatRaster     | Binary raster with the desired resolution, crs and extent |
| sch_202707 | raster | SpatRaster                           | Level 2 Sentinel-2 Szene containing the example area |
| sch_dgm | raster |   SpatRaster               | Digital Ground Model        |
| Station_G06 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_G17 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_G20 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_G21 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_G25 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_G48 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_W10 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_W11 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_W19 | tabular | dataframe | Imaginary climate station data from imaginary climate station |
| Station_W20 | tabular | dataframe | Imaginary climate station data from imaginary climate station |



