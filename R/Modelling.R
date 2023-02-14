#' Modelling
#'
#' Creates Models for each climate value
#'
#' @param classifier vector or character. Model variants to be used. Supported models: Random Forest = "rf", Partial-Least-Squares = "pls", Neural Networks = "nnet", Linear Regression = "lm" or generalized boosted regression = "gbm".
#' @param fold character. Saving Algorithm. Either folding over location only "LLO", over time only "LTO", or over both "LLTO".
#' @param k integer. When 'fold' = "LLO" or "LTO". Set k to the number of unique spatial or temporal units.
#' @param tc_method character. Method for train controll function from caret package. Default = "cv".
#' @param metric character. See "train".
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
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
                       fold = "LLO",
                       predrows = NULL,
                       mnote = NULL,
                       k = NULL,
                       tc_method = "cv",
                       metric = "RMSE",
                       doParallel = FALSE,
                       ...){
  library(DescTools)

  data_o <- read.csv(file.path(envrmt$path_tfinal, "final_monthly.csv"));
  df_total <- data.frame();

  for (y in timespan) try  ({

    data_y <- data_o[data_o$year %like% y, ]
    data <- data_y[complete.cases(data_y), ]
    time <- y
    print("Training models for year ", time)


    for (s in climresp) try({
      set.seed(seed)
      partition_indexes <- caret::createDataPartition(data$plot,
                                                      times = 1,
                                                      p = p,
                                                      list = FALSE)
      trainingDat <- data[partition_indexes, ]
      testingDat <- data[-partition_indexes, ]

      save(trainingDat, file = file.path(envrmt$path_tfinal, paste0(time, mnote, "_", "trainingDat.RData")))
      save(testingDat, file = file.path(envrmt$path_tfinal, paste0(time, mnote, "_", "testingDat.RData")))

      if (fold == "LLO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, spacevar = "plot", k = 3) #set k to the number of unique spatial or temporal units. (k = 3)
      }

      if (fold == "LTO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, timevar = "datetime") #set k to the number of unique spatial or temporal units. (k = 12)
      }

      if (fold == "LLTO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, timevar= "datetime", spacevar = "plot")
      }

      ctrl <- caret::trainControl(method = "cv",
                                  index = folds$index,
                                  savePredictions=TRUE
                                  )

      preds <- trainingDat[ ,predrows]

      resps <- trainingDat[ ,s]
      if (s == 6){sensor = "tem"}
      if (s == 9){sensor = "reh"}
      if (s == 12){sensor = "pre"}
      if (s == 13){sensor = "sun"}
      print(paste0(colnames(trainingDat[s]), " -> ", sensor))

      for (i in 1:length(classifier)) try ({
        method = classifier[i]
        print("method = ", method)
        tuneGrid <- NULL

        if (method == "gbm"){
          tuneLength <- 10
        }
        if (method == "lm"){
          tuneLength <- 10
        }
        if (method == "rf"){
          tuneLength <- 1
          tuneGrid <- expand.grid(mtry = 2)
        }
        if (method == "pls"){
          preds <- data.frame(scale(preds))
          tuneLength <- 10
        }
        if (method == "nnet"){
          tuneLength <- 1
          preds <- data.frame(scale(preds))
          tuneGrid <- expand.grid(size = seq(2,ncol(preds),2),
                                  decay = seq(0,0.1,0.025)
                                  )
        }

        if (doParallel == TRUE){
          cr <- parallel::detectCores()
          cl <- parallel::makeCluster(cr-4)
          doParallel::registerDoParallel(cl)
        }

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

        if (doParallel == TRUE){
          stopCluster(cl)
        }

        saveRDS(ffsmodel, file.path(envrmt$path_models, paste0(time, "_", fold, "_", mnote, "_", classifier[i], "_", sensor, "_ffs_model.rds")))

        mod <- ffsmodel
        accuracy <- min(mod$selectedvars_perf)

        df <- data.frame(
          year_month = time,
          classifier = method,
          accuracy = accuracy,
          Nrmse = accuracy / (max(resps) - min(resps)),
          sensor = sensor,
          modeltype = fold,
          note = mnote
        )

        df$variables[1] <- list(c(mod$selectedvars))
        df_total <- rbind(df_total,df)

        remove(data_y, data_ym, ffsmodel, idx_y, idx_ym, mod)
        gc()
      })
    })
  })

  # save total loop analytics for eval
  saveRDS(df_total, file.path(envrmt$path_statistics, paste0(fold, mnote, "_eval_df.rds")));
  return(df_total)
}
