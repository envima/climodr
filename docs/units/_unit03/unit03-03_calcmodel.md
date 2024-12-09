---
title: calc.model
header:
  image: '/assets/images/teaserimages/felder.png'
  caption: '[Marco Verch via ccnull.de](https://ccnull.de/foto/drohnenaufnahme-von-landwirtschaftlichen-feldern-mit-geometrischen-mustern/1105470). [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/de/). Image cropped.'
---

## Modelling

### Description
Creates Models for each climate value

### Usage
```r
calc.model(
  method = "monthly",
  timespan,
  climresp,
  classifier = c("rf", "pls", "lm", "glm"),
  seed = NULL,
  p = 0.8,
  folds = "all",
  predrows,
  mnote = NULL,
  k = NULL,
  tc_method = "cv",
  metric = "RMSE",
  doParallel = FALSE,
  autocorrelation = FALSE,
  ...
)
```

### Arguments
**timespan** 		numeric. Vector or single input. Should contain all years to be modeled. The years have to be the same format as in the tabular data.  
**climresp** 		numeric. Vector or single input. Should contain all column's in the tabular data that contain response variables.  
**classifier** 		vector or character. Model variants to be used. Supported models: Random Forest = "rf", Partial-Least-Squares = "pls", Neural Networks = "nnet", Linear Regression = "lm" or generalized boosted regression = "gbm".  
**seed** 			integer. Seed to reproduce the same model over and over.  
**folds** 			character. Vector or single input. Either folding over location only "LLO", over time only "LTO", or over both "LLTO". Use "all" to use all possibilitys.  
**predrows** 		numeric. Vector or single input. Should contain the rows where all the predictor values are stored in.  
**mnote** 			character. Model note for special modifications used. Default: "normal"  
**k**				integer. When 'fold' = "LLO" or "LTO". Set k to the number of unique spatial or temporal units. Leave out to use preset values.  
**tc_method**		character. Method for train control function from caret package. Default = "cv".  
**metric**			character. See 'train'.
**doParallel**		logical. Parallelization accelerates the modelling process. Warning: Your PC will slow down drastically. Make sure to not run any other heavy processes during this.  
**autocorrelation**	logical. Should autocorrelating data in the predictor variables be excluded from the model run? Only works if 'autocorr' has been executed beforehand.  

### Value
Data Frame (Evaluation Table)
Models (Count depends on your settings, saved in "/workflow/models"

### See Also
`autocorr`

## Examples
```r
## Not run: 
# Create 48 different models (12 months x 4 classifiers) for every month in 2017
calc.model(method = "monthly",
           timespan = c(2017),
           climresp = 5,
           classifier = c("rf",
                          "pls",
                          "nnet",
                          "lm"),
           seed = 707,
           p = 0.8,
           folds = "LLO",
           mnote = "vignette",
           predrows = c(8:24),
           tc_method = "cv",
           metric = "RMSE",
           autocorrelation = TRUE,
           doParallel = FALSE)

## End(Not run)
```
