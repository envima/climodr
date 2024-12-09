---
title: prep.csv
toc: true
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Preparing CSV-Data

### Description
Crops input data to the extent size and removes NA-Values

### Usage
```r
prep.csv(method = "proc", save_output = TRUE, ...)
```

### Arguments
**method**		character. "proc" for ready-to-use data in separate .csv-files. "tube" for raw-data from the Tube Data Base. Default "proc"-Method.  
**save_output**	logical. If cleaned data should be saved permanently in the Environment put save_output = TRUE. Otherwise the output will be saved in the temporary directory. Default: FALSE.  

### Value
Data Frame

### See also
`proc.csv`, `spat.csv`, `fin.csv`

## Examples

```r
## Not run: 
prep.csv(method = "proc", save_output = TRUE)

#check the created csv files
csv_files <- grep("_no_NAs.csv$",
                  list.files(envrmt$path_tworkflow),
                  value=TRUE)
csv_files

## End(Not run)
```
