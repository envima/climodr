#' Modelling
#'
#' Creates Models for each climate value
#'
#' @param fold character. Saving Algorithm. Either folding over location only "LLO", over time only "LTO", or over both "LLTO".
#' @param safe_output logical. If cleaned data should be safed permanently in the Environment put safe_output = TRUE.
#' Otherwise the output will be safed in the temporary directory. Default: FALSE.
#'
#' @return modell
#' @seealso
#'
#' @name calc.model
#' @export calc.model
#'
#' @examples
#'
calc.model <- function(timespan = NULL,
                       response = NULL,
                       classifier = NULL,
                       seed = NULL,
                       times = NULL,
                       fold = "LLO",
                       predrows = NULL,
                       ...){
  data_o <- read.csv(file.path(envrmt$path_data,"final_clim_df.csv"));
  df_total = data.frame();

  for (y in timespan) try({

    data_y=data_o[data_o$year %like% y, ]
    data=data_y[complete.cases(data_y), ]
    time=y
    print(time)

    for (s in response) try({
      set.seed(seed)
      partition_indexes = createDataPartition(data$plotID,
                                              times = times,
                                              p = p,
                                              list = FALSE)
      trainingDat = data[partition_indexes, ]
      testingDat = data[-partition_indexes, ]

      # Saving Algorithm needs to be added

      if (fold == "LLO"){
        folds = CreateSpacetimeFolds(trainingDat,spacevar = "plotID",k=3)#set k to the number of unique spatial or temporal units.
      }

      if (fold == "LTO"){
        folds = CreateSpacetimeFolds(trainingDat,timevar = "datetime",k=12)#set k to the number of unique spatial or temporal units.
      }

      if (fold == "LLTO"){
        folds = CreateSpacetimeFolds(trainingDat,timevar= "datetime", spacevar = "plotID")#set k to the number of unique spatial or temporal units.
      }

      ctrl = trainControl(method = "cv",index = folds$index,
                        savePredictions=TRUE)


      predictors=trainingDat[,predrows]

      response = trainingDat[,s]
      print(colnames(trainingDat[s]))
      sensor=colnames(trainingDat[s])

        for (i in 1:length(classifier))try({
          ctrl = trainControl(method = "cv",
                              index = folds$index,
                              indexOut = folds$indexOut,

      )
      classifier= c("rf", "pls","nnet" ,"lm")
      #i=1 # rf

      method = classifier[i]
      print(method)

      tuneLength = 2
      tuneGrid = NULL

      if (method=="gbm"){
        tuneLength = 10
      }
      if (method=="lm"){
        tuneLength = 10
      }
      if (method=="rf"){
        tuneLength = 1
        tuneGrid = expand.grid(mtry = 2)
      }
      if (method=="pls"){
        predictors = data.frame(scale(predictors))
        tuneLength = 10
      }
      if (method=="nnet"){
        tuneLength = 1
        predictors = data.frame(scale(predictors))
        tuneGrid = expand.grid(size = seq(2,ncol(predictors),2),
                               decay = seq(0,0.1,0.025))
      }

      cr=detectCores()
      cl = makeCluster(cr-4)
      registerDoParallel(cl)

      ffsmodel = ffs(predictors,
                     response,
                     metric="RMSE",
                     withinSE = TRUE,
                     method = method,
                     importance =TRUE,
                     tuneLength = tuneLength,
                     tuneGrid = tuneGrid,
                     trControl = ctrl,
                     trace = FALSE, #relevant for nnet
                     linout = TRUE,
                     verbose=FALSE) #relevant for nnet

      stopCluster(cl)

      # save model
      saveRDS(ffsmodel,paste0(save,time,fold,classifier[i],"_",sensor,"_ffs_model.rds"))

      ####save model stats and make mini df for later analysis
      mod=ffsmodel
      accuracy=min(mod$selectedvars_perf)
      #make df with cols and read the model info
      df=data.frame(
        year_month=time,
        classifier=mod$method,
        accuracy=accuracy,
        Nrmse=accuracy/(max(response)-min(response) ),
        sensor=sensor
      )
      #add the vars in list
      df$variables[1]=list(c(mod$selectedvars))

      #make big df of all from this run
      df_total = rbind(df_total,df)

      #cleanup
      remove(data_y,data_ym,ffsmodel,idx_y,idx_ym,mod,testingDat,trainingDat)
      gc()

    })})

  # save total loop analytics for eval
  saveRDS(df_total,paste0(save,fold,"eval_df.rds"))

}
