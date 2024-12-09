---
title: climplot
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Create Maps using the 'terra' package graphic parameters

### Description
Plot results of climodr into maps. Right now maps are created using the terra package. The maps created are very basic. Will be updated to run with tidyterra in future.

### Usage 
```r
climplot(
  mnote,
  sensor,
  aoa = FALSE,
  mapcolors = rev(grDevices::terrain.colors(50)),
  scale_position = "bottomleft",
  north_position = "topright"
)
```

### Arguments
**mnote** 			character. The modelnote you want to create maps of.  
**sensor**			character. The sensor you want to create maps for.  
**aoa**				logical. Do you want the area of applicability to be added to your map?  
**mapcolors**		The color pallete you want to use for the map. Default is `rev(grDevices::terrain.colors(50))`  
**scale_position**	character. Graphical parameter. The relative positiion of the Scale for the map. See 'terra::plot' for more details.  
**north_position** 	character. Graphical parameter. The relative positiion of the Scale for the map. See 'terra::plot' for more details.  

### Value
Maps in PNG-Format to your harddrive at "/output/maps"

### See Also
[`plot`](https://rspatial.github.io/terra/reference/plot.html)

## Examples
```r
## Not run: 
# Create a Temperature Map from the vignette model
climplot(mnote = "vignette",
         sensor = "Ta_200",
         aoa = FALSE,
         mapcolors = rev(heat.colors(50)),
         scale_position = "bottomleft",
         north_position = "topright")

## End(Not run)
```