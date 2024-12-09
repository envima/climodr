---
title: proc.csv
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Processing CSV-Data

### Description
Calculate averaged sensor values aggregated to a given time interval.

### Usage
```r
proc.csv(method = "monthly", rbind = TRUE, save_output = TRUE, ...)
```

### Arguments
**method**		character. Either "daily", monthly" or "annual". Also depends on the available data.  
**rbind**		logical. Create a single file with all climate stations. If FALSE, every station will be saved in a seperate file.  
**save_output**	logical. If data should be saved permanently in the Environment put save_output = TRUE. Otherwise the output will be saved in the temporary directory. Default: TRUE.  

### Value
Data Frame

### See also
`prep.csv`, `spat.csv`, `fin.csv`

## Examples

```r
## Not run: 
csv_data <- proc.csv(method = "monthly",
                     rbind = TRUE,
                     save_output = TRUE)
head(csv_data)

## End(Not run)
```
