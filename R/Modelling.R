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
                       response,
                       classifier = c("rf", "pls","nnet" ,"lm"),
                       seed = NULL,
                       p = 0.8,
                       fold = "LLO",
                       predrows = NULL,
                       k = NULL,
                       tc_method = "cv",
                       metric = "RMSE",
                       doParallel = FALSE,
                       ...){
  data_o <- read.csv(file.path(envrmt$path_tfinal, "final_monthly.csv"));
  df_total <- data.frame();
  library(DescTools)

  for (y in timespan) try  ({

    data_y <- data_o[data_o$year %like% y, ]
    data <- data_y[complete.cases(data_y), ]
    time <- y
    print(time)


    for (s in response) try({
      set.seed(seed)
      partition_indexes <- caret::createDataPartition(data$plot,
                                                      times = 1,
                                                      p = p,
                                                      list = FALSE)
      trainingDat <- data[partition_indexes, ]
      testingDat <- data[-partition_indexes, ]

      # Saving Algorithm needs to be added

      if (fold == "LLO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, spacevar = "plot", k = k) #set k to the number of unique spatial or temporal units. (k = 3)
      }

      if (fold == "LTO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, timevar = "datetime", k = k) #set k to the number of unique spatial or temporal units. (k = 12)
      }

      if (fold == "LLTO"){
        folds <- CAST::CreateSpacetimeFolds(trainingDat, timevar= "datetime", spacevar = "plotID")
      }

      ctrl <- caret::trainControl(method = "cv",
                                  index = folds$index,
                                  savePredictions=TRUE
                                  )

      predictors <- trainingDat[ ,predrows]

      response <- trainingDat[ ,s]
      print(colnames(trainingDat[s]))
      sensor <- colnames(trainingDat[s])

      for (i in 1:length(classifier)) try ({
        ctrl <- caret::trainControl(
          method = tc_method,
          index = folds$index,
          indexOut = folds$indexOut
        )

        method = classifier[i]
        print(method)

        tuneLength = 2
        tuneGrid = NULL

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
          predictors <- data.frame(scale(predictors))
          tuneLength <- 10
        }
        if (method == "nnet"){
          tuneLength <- 1
          predictors <- data.frame(scale(predictors))
          tuneGrid <- expand.grid(size = seq(2,ncol(predictors),2),
                                  decay = seq(0,0.1,0.025)
                                  )
          ifnnet <- TRUE
        } else {
          ifnnet <- FALSE
        }

        if (doParallel == TRUE){
          cr <- parallel::detectCores()
          cl <- parallel::makeCluster(cr-4)
          doParallel::registerDoParallel(cl)
        }

        ffsmodel <- CAST::ffs(predictors,
                              response,
                              metric = metric,
                              withinSE = TRUE,
                              method = method,
                              importance =TRUE,
                              tuneLength = tuneLength,
                              tuneGrid = tuneGrid,
                              trControl = ctrl,
                              trace = ifnnet,
                              linout = TRUE,
                              verbose = ifnnet)

        if (doParallel == TRUE){
          stopCluster(cl)
        }

        saveRDS(ffsmodel, file.path(envrmt$path_models, paste0(time, fold, classifier[i], "_", sensor, "_ffs_model.rds")))

        mod <- ffsmodel
        accuracy <- min(mod$selectedvars_perf)

        df <- data.frame(year_month = time,
                         classifier = mod$method,
                         accuracy = accuracy,
                         Nrmse = accuracy / (max(response) - min(response)),
                         sensor = sensor
        )

        df$variables[1] <- list(c(mod$selectedvars))
        df_total <- rbind(df_total,df)

        remove(data_y, data_ym, ffsmodel, idx_y, idx_ym, mod, testingDat, trainingDat)
        gc()
      })
    })
  })

  # save total loop analytics for eval
  saveRDS(df_total, file.path(envrmt$path_statistics, paste0(fold, "_eval_df.rds")));
  return(df_total)
}
