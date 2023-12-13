#' Modelling
#'
#' Creates Models for each climate value
#'
#' @param timespan vector or integer. Should contain all years to be modeled.
#'                 The years have to be the same format as in the tabular data.
#' @param climresp vector or integer. Should contain all response column's in
#'                 the tabular data.
#' @param classifier vector or character. Model variants to be used. Supported
#'                 models: Random Forest = "rf", Partial-Least-Squares = "pls",
#'                 Neural Networks = "nnet", Linear Regression = "lm" or
#'                 generalized boosted regression = "gbm".
#' @param seed     integer. Seed to reproduce the same model over and over. e.g.
#'                 "seed = 707"
#' @param fold     character. Saving Algorithm. Either folding over location
#'                 only "LLO", over time only "LTO", or over both "LLTO".
#' @param predrows vector or integer. Should contain the rows where all the
#'                 predictor values are stored in.
#' @param mnote    character. Model note for special modifications used.
#'                 Default: "normal"
#' @param k        integer. When 'fold' = "LLO" or "LTO". Set k to the number
#'                 of unique spatial or temporal units.
#' @param tc_method character. Method for train control function from caret
#'                 package. Default = "cv".
#' @param metric   character. See "train".
#' @param doParallel logical. Parallelization accelerates the modelling
#'                 process. Warning: Your PC will slow down drastically. Make
#'                 sure to not run any other heavy processes during this.
#' @param safe_output logical. If cleaned data should be safed permanently in
#'                 the Environment put safe_output = TRUE. Otherwise the output
#'                 will be safed in the temporary directory. Default: FALSE.
#'
#' @return data frame.
#' @seealso
#'
#' @name calc.model
#' @export calc.model
#'
#' @examples
#'
calc.model <- function(timespan,
                       climresp,
                       classifier = c("rf", "pls","nnet" ,"lm"),
                       seed = NULL,
                       p = 0.8,
                       folds = "all",
                       predrows = NULL,
                       mnote = NULL,
                       k = NULL,
                       tc_method = "cv",
                       metric = "RMSE",
                       doParallel = FALSE,
                       autocorrelation = FALSE,
                       ...){
  data_o <- read.csv(file.path(envrmt$path_tfinal, "final_daily.csv"));
  df_total <- data.frame();

  # for convenience to the user; all means all folds will be calculated.
  if (folds == "all"){
    folds <- c("LLO", "LTO", "LLTO")
  }

  for (y in timespan) try  ({

    data_y <- data_o[c(which(data_o$year == y)), ]
    data_y <- data_y[complete.cases(data_y), ]
    months <- unique(data_y$month)

    for (m in months) try ({
      data_m <- data_y[c(which(data_y$month == m)), ]
      print(paste0("Training monthly models for ", y,".  ", m, "/", length(months)))

      for (s in climresp) try({
        set.seed(seed)

        if(autocorrelation == "TRUE"){

          data <- data_m[complete.cases(data_m), ]

          if (s == climresp[1]){
            delect <- read.csv(file.path(envrmt$path_statistics, "tem_delect.csv"))
          }
          if (s == climresp[2]){
            delect <- read.csv(file.path(envrmt$path_statistics, "reh_delect.csv"))
          }
#          if (s == climresp[3]) try ({
#            delect <- read.csv(file.path(envrmt$path_statistics, "pre_delect.csv"))
#          })
#          if (s == climresp[4]) try ({
#            delect <- read.csv(file.path(envrmt$path_statistics, "sun_delect.csv"))
#          })

          if (!(length(delect$variables) == 0)){
            data <- data %>% dplyr::select(-c(delect$variables))
          }
        }

        partition_indexes <- caret::createDataPartition(data$plot,
                                                        times = 1,
                                                        p = p,
                                                        list = FALSE)

        trainingDat <- data[partition_indexes, ]
        testingDat <- data[-partition_indexes, ]

        for (f in 1:length(folds)) try( {

          set.seed(seed)

          if (folds[f] == "LLO"){
            fold <- CAST::CreateSpacetimeFolds(trainingDat, spacevar = "plot")
          }
          if (folds[f] == "LTO"){
            fold <- CAST::CreateSpacetimeFolds(trainingDat, timevar = "datetime")
          }
          if (folds[f] == "LLTO"){
            fold <- CAST::CreateSpacetimeFolds(trainingDat, timevar= "datetime", spacevar = "plot")
          }

          ctrl <- caret::trainControl(method = "cv",
                                      index = fold$index,
                                      savePredictions = TRUE)

          if (autocorrelation == TRUE){
            preds <- trainingDat[ ,head(predrows, -length(delect$variables))]
          } else {
            preds <- trainingDat[ ,predrows]
          } # end autocorrelation-loop

          # set response
          resps <- trainingDat[ ,s]

          # define sensors
          if (s == climresp[1]){sensor <- "tem"}
          if (s == climresp[2]){sensor <- "reh"}
#         if (s == climresp[3]){sensor <- "sun"}
#         if (s == climresp[4]){sensor <- "pre"}
            print(paste0('The response column ',
                         colnames(trainingDat[s]),
                         ' will be safed as ',
                         sensor, '.'))

          # save the training and testing data for further evaluation
          save(trainingDat, file = file.path(envrmt$path_tfinal, paste0(y, m, "_", mnote, "_", sensor, "_", "trainingDat.RData")))
          save(testingDat, file = file.path(envrmt$path_tfinal, paste0(y, m, "_", mnote, "_", sensor, "_", "testingDat.RData")))

          # start classifier loop
          for (i in 1:length(classifier)) try ({

            method <- classifier[i]
            print(paste0("Next model: ", method))
            tuneLength = 2
            tuneGrid <- NULL

            # adjust settings for different model types
            if (method == "gbm"){
              tuneLength <- 10
              ctrl = trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 10,
                                  savePredictions = TRUE)
              modclass <-"gbm"
            }
            if (method == "lm"){
              tuneLength <- 10
              modclass <- "lim"
            }
            if (method == "rf"){
              tuneLength <- 1
              tuneGrid <- expand.grid(mtry = 2)
              modclass <- "raf"
            }
            if (method == "pls"){
#              preds <- data.frame(scale(preds))
              tuneLength <- 10
              modclass <- "pls"
            }
            if (method == "nnet"){
              tuneLength <- 1
              preds <- data.frame(scale(preds))
              tuneGrid <- expand.grid(size = seq(2,ncol(preds),2),
                                      decay = seq(0,0.1,0.025)
              )
              modclass <- "nnt"
            }

            # Optional: activate parallelisation for faster computing
            if (doParallel == TRUE){
              cr <- parallel::detectCores()
              cl <- parallel::makeCluster(cr - 2)
              doParallel::registerDoParallel(cl)
            }

            # calculate model
            ffsmodel <- CAST::ffs(predictors = preds,
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

            # stop paralellization, if it was activated
            if (doParallel == TRUE){
              stopCluster(cl)
            }

            # save all models
            saveRDS(ffsmodel, file.path(envrmt$path_models, paste0(y, m, "_", fold, "_", mnote, "_", modclass, "_", sensor, "_ffs_model.rds")))

            # create data frame for model evaluation
            if (method == "gbm"){
              accuracy = min(ffsmodel$results$RMSE)
            } else {
              accuracy <- min(ffsmodel$selectedvars_perf)
            }

            df <- data.frame(
              year_month = paste0(y, m),
              classifier = modclass,
              accuracy = accuracy,
              Nrmse = accuracy / (max(resps) - min(resps)),
              Rsqrd = ffsmodel$results[1,3],
              sensor = sensor,
              modeltype = folds[f],
              note = mnote)

          #add the vars in list
            if (method == "gbm"){
              df$variables[1] = list(c(colnames(ffsmodel$ptype)))
            } else {
              df$variables[1] = list(c(ffsmodel$selectedvars))
            }

            df_total <- rbind(df_total, df)

            remove(ffsmodel)
            gc()
          }) # end classifier loop (modeltype)
        }) # end fold loop
      }) # end climresp loop  (response sensor)
    }) # end months loop
  }) # end timespan loop (year, month, etc.)

  # save total loop analytics for eval
  saveRDS(df_total, file.path(envrmt$path_statistics, paste0(mnote, "_mod_eval_df.rds")));
  return(df_total)
}
