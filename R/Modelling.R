#' Modelling
#'
#' Creates Models for each climate value
#'
#' @param timespan numeric. Vector or single input. Should contain all years to
#'                 be modeled. The years have to be the same format as in the
#'                 tabular data.
#' @param climresp numeric. Vector or single input. Should contain all column's
#'                 in the tabular data that contain response variables.
#' @param classifier vector or character. Model variants to be used. Supported
#'                 models: Random Forest = "rf", Partial-Least-Squares = "pls",
#'                 Neural Networks = "nnet", Linear Regression = "lm" or
#'                 generalized boosted regression = "gbm".
#' @param seed     integer. Seed to reproduce the same model over and over.
#' @param folds    character. Vector or single input. Either folding over location
#'                 only "LLO", over time only "LTO", or over both "LLTO". Use
#'                 "all" to use all possibilitys.
#' @param predrows numeric. Vector or single input. Should contain the rows where
#'                 all the predictor values are stored in.
#' @param mnote    character. Model note for special modifications used.
#'                 Default: "normal"
#' @param k        integer. When 'fold' = "LLO" or "LTO". Set k to the number
#'                 of unique spatial or temporal units.  Leave out to use preset
#'                 values.
#' @param tc_method character. Method for train control function from caret
#'                 package. Default = "cv".
#' @param metric   character. See `train`.
#' @param doParallel logical. Parallelization accelerates the modelling
#'                 process. Warning: Your PC will slow down drastically. Make
#'                 sure to not run any other heavy processes during this.
#' @param autocorrelation logical. Should autocorrelating data in the predictor
#'                 variables be excluded from the model run? Only works if
#'                 `autocorr` has been executed beforehand.
#'
#' @return data frame.
#' @seealso `autocorr`
#'
#' @name calc.model
#' @export calc.model
#'
#' @examples
#'
calc.model <- function(
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
    ...)
{
  data_o <- read.csv(
    file.path(
      envrmt$path_tfinal,
      paste0(
        "final_",
        method,
        ".csv"
        )
      )
    )
  df_total <- data.frame()

# ------------------------- Parallelisation --------------------------------- #
  # Optional: activate for faster computing
  if (doParallel == TRUE){
    # talk to the user
    print("Starting parallelization.")

    cr <- parallel::detectCores()
    cl <- parallel::makeCluster(cr * 0.75)
    doParallel::registerDoParallel(cl)
  }

# --------------------------------------------------------------------------- #

  # for convenience to the user; all means all folds will be calculated.
  if (folds == "all"){
    dofolds <- c("LLO", "LTO", "LLTO")
  } else {
    dofolds <- folds
  }

# Beginning of Y-Loop ------------------------------------------ Timespan --- #
  for (y in timespan) try  ({

    data_y <- data_o[c(which(data_o$year == y)), ]
    data_y <- data_y[complete.cases(data_y), ]
    months <- unique(data_y$month)

    # talk to the user
    print(paste0("Training models for ", y, ".  Year-Nr.: ", which(timespan == y), "/", length(timespan)))

# Loop for monthly models ----------------------------------------- Month --- #
    for (m in months) try ({
      data_m <- data_y[c(which(data_y$month == m)), ]

      # talk to the user
      print(paste0("Training monthly models for ", y,".  Month-Nr.: ", m))

# Loop for the climate sensors --------------------------------- Climresp --- #
      for (s in climresp) try({

        if (!is.null(seed)){set.seed(seed)}

## Autocorellation Condition ------------------------------------------------ #
        sensor_names <- readRDS(
          file.path(
            envrmt$path_tmp,
            "sensor_names.rds"
          )
        )

        if(autocorrelation == "TRUE"){
          # talk to the user
          print("Use autocorellation data for filtering..")
          data <- data_m[complete.cases(data_m), ]
          delect <-
            read.csv(
              file.path(
                envrmt$path_statistics,
                paste0(
                  sensor_names[which(s == climresp)],
                  "_delect.csv"
                  )
                )
              )

          if (!(length(delect$variables) == 0)){
            data <- data %>%
              dplyr::select(-c(delect$variables))
          }
        } else {
          data <- data_m[
            complete.cases(data_m), ]
        } # end autocorrelation loop

# Create Training and Test Data --------------------------------------------- #

        partition_indexes <- caret::createDataPartition(data$plot,
                                                        times = 1,
                                                        p = p,
                                                        list = FALSE)

        trainingDat <- data[partition_indexes, ]
        testingDat <- data[-partition_indexes, ]

# Loop for SpaceTimeFolds --------------------------------------- DoFolds --- #
        for (fo in 1:length(dofolds)){
          f <- dofolds[fo]
          if(!is.null(seed)){set.seed(seed)}
          if (f == "LLO"){
            fold <- CAST::CreateSpacetimeFolds(
              trainingDat,
              spacevar = "plot"
              )
            # talk to the user
            print(
              paste0(
                "Run with spatial folds for cross validation.  Fold-Nr.: ",
                which(f == dofolds), "/", length(dofolds)
                )
              )
            } # end LLO
          if (f == "LTO"){
            fold <- CAST::CreateSpacetimeFolds(
              trainingDat,
              timevar = "datetime"
              )
            # talk to the user
            print(
              paste0(
                "Run with temporal folds for cross validation.  Fold-Nr.: ",
                which(f == dofolds), "/", length(dofolds)
                )
              )
            } # end LTO
          if (f == "LLTO"){
            fold <- CAST::CreateSpacetimeFolds(
              trainingDat,
              timevar = "datetime",
              spacevar = "plot"
            )
            # talk to the user
            print(
              paste0(
                "Run with spatio-temporal folds for cross validation.  Fold-Nr.: ",
                which(f == dofolds), "/", length(dofolds)
                )
              )
            } # end LLTO
          ctrl <- caret::trainControl(
            method = "cv",
            index = fold$index,
            savePredictions = TRUE
            )
          if (autocorrelation == TRUE){
            preds <- trainingDat[, head(predrows,
                                        -length(delect$variables)
                                        )
                                 ]
          } else {
            preds <- trainingDat[, predrows]
          } # end autocorrelation-loop

          # set response
          resps <- trainingDat[ ,s]

          # talk to the user
          print(
            paste0(
              "Calculate models for sensor: ",
              sensor_names[which(s == climresp)])
            )

          # save the training and testing data for further evaluation
          save(
            trainingDat,
            file = file.path(
              envrmt$path_tfinal,
              paste0(
                y,
                m,
                "_",
                mnote,
                "_",
                sensor_names[which(s == climresp)],
                "_trainingDat.RData"
                )
              )
            )
          save(
            testingDat,
            file = file.path(
              envrmt$path_tfinal,
              paste0(
                y,
                m,
                "_",
                mnote,
                "_",
                sensor_names[which(s == climresp)],
                "_testingDat.RData")
              )
            )

# Loop for Classifiers --------------------------------------- Classifier --- #

          # start classifier loop
          for (i in 1:length(classifier)) try ({
            method <- classifier[i]
            tuneLength = 2
            tuneGrid <- NULL

            # adjust settings for different model types
            if (method == "gbm"){
              tuneLength <- 10
              ctrl <- caret::trainControl(
                method = "repeatedcv",
                number = 10,
                repeats = 10,
                savePredictions = TRUE)
              modclass <-"gbm"
              # talk to the user
              print(paste0("Next model: Stochastic Gradient Boosting.  ", i, "/", length(classifier)))
            }
            if (method == "lm"){
              tuneLength <- 10
              modclass <- "lim"
              # talk to the user
              print(paste0("Next model: Linear Regression.  ", i, "/", length(classifier)))
            }
            if (method == "rf"){
              tuneLength <- 1
              tuneGrid <- expand.grid(mtry = 2)
              modclass <- "raf"
              # talk to the user
              print(paste0("Next model: Random Forest.  ", i, "/", length(classifier)))
            }
            if (method == "pls"){
#              preds <- data.frame(scale(preds))
              tuneLength <- 10
              modclass <- "pls"
              # talk to the user
              print(paste0("Next model: Partial-Least-Squares.  ", i, "/", length(classifier)))
            }
            if (method == "nnet"){
              tuneLength <- 1
              tuneGrid <- expand.grid(size = seq(2,ncol(preds),2),
                                      decay = seq(0,0.1,0.025)
              )
              modclass <- "nnt"
              # talk to the user
              print(paste0("Next model: Neural Networks.  ", i, "/", length(classifier)))
            }

            # talk to the user
            print("Computing model...")

            # calculate model
            ffsmodel <- CAST::ffs(
              predictors = preds,
              response = resps,
              metric = "RMSE",
              withinSE = FALSE,
              method = method,
              importance = TRUE,
              tuneLength = tuneLength,
              tuneGrid = tuneGrid,
              trControl = ctrl,
              linout = TRUE,
              verbose = FALSE,
              trace = FALSE
            )

            date <- ifelse(
              m < 10,
              paste0(y, "0", m),
              paste0(y, m)
            )

            # save all models
            saveRDS(
              ffsmodel,
              file.path(
                envrmt$path_models,
                paste0(
                  mnote,
                  "_",
                  sensor_names[which(s == climresp)],
                  "_",
                  date,
                  "_",
                  f,
                  "_",
                  modclass,
                  "_ffs_model.rds")))

# Evaluation Data Frame ----------------------------------------------------- #
            # create data frame for model evaluation
            if (method == "gbm"){
              accuracy = min(ffsmodel$results$RMSE)
            } else {
              accuracy <- min(ffsmodel$selectedvars_perf)
            }

            df <- data.frame(
              year_month = date,
              classifier = modclass,
              accuracy = accuracy,
              Nrmse = accuracy / (max(resps) - min(resps)),
              Rsqrd = ffsmodel$results[1,3],
              sensor = sensor_names[which(s == climresp)],
              modeltype = f,
              note = mnote)

          #add the vars in list
            if (method == "gbm"){
              df$variables[1] = list(c(colnames(ffsmodel$ptype)))
            } else {
              df$variables[1] = list(c(ffsmodel$selectedvars))
            }

            df_total <- rbind(df_total, df)

#           remove(ffsmodel)
#           gc()
          }) # end classifier loop [i]
        } # end fold loop [f]
      }) # end climresp loop [s]
    }) # end months loop [m]
  }) # end timespan loop [y]

  #talk to the user
  print("Done! Saving evaluation data frame.")

  # save total loop analytics for eval
  saveRDS(df_total, file.path(envrmt$path_statistics, paste0(mnote, "_mod_eval_df.rds")));
  return(df_total)

  # stop paralellization, if it was activated
  if (doParallel == TRUE){
    #talk to the user
    print("Ending parallelization.")
    parallel::stopCluster(cl)
  }

}
