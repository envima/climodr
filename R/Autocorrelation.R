#' Autocorrelation
#'
#' Tests the final.csv created with `fin.csv` on autocorrelation to produce
#' reliable models.
#'
#' @param envrmt variable name of your envrmt list created using climodr's `envi.create` function. Default = envrmt.
#' @param method character. Choose the time scale your data is preserved in. Either "annual", "monthly" or "daily".
#' @param resp numerical. Vector or single input of the columns in the final.csv that contain your sensor data ("response variables"). The function will create one file per variable.
#' @param pred numerical. Vector or single input. The columns of your predictor variables, that you want to test for autocorrelation with the response variables.
#' @param plot.corrplot logical. Should correlation matrices be plotted?
#' @param corrplot character. Vector or single input. If plot.corrplot is true, you can choose the design of the correlation plot. You can choose from "coef", "crossout", "blank". Default is "coef".
#'
#' @return One .csv file per response variable. These will later be used when `autocorrelation` is set `TRUE` during `calc.model`.
#' @seealso `calc.model`
#'
#' @name autocorr
#' @export autocorr
#'
#' @examples
#' \dontrun{
#' # Test data for autocorrelation after running fin.csv
#' autocorr(method = "monthly",
#'          resp = 5,
#'          pred = c(8:24),
#'          plot.corrplot = FALSE)
#' }
#'

autocorr <- function(
    envrmt = .GlobalEnv$envrmt,
    method = "monthly",
    resp,
    pred,
    plot.corrplot = TRUE,
    corrplot = "coef")
  {
  #get data from PreProcessing
  data_o <- utils::read.csv(
    file.path(
      envrmt$path_tfinal,
      paste0(
        "final_",
        method,
        ".csv"
        )
      )
    )

  #subset data
  data <- data_o[ ,c(resp, pred)]

  #duplicated data
  #data_d <- data[duplicated(data),];
  data <- data[stats::complete.cases(data), ]

  c <- stats::cor(data)

  ### -- smol function from stack overflow for cor-test -- ##
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- stats::cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  };

  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(data)
  round(utils::head(p.mat[, 1:5]), 6)

  if (plot.corrplot == TRUE){
    col <- grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#fff0db", "#77AADD", "#4477AA"));

    if ("coef" %in% corrplot){
      corrplot::corrplot(c,
                         method = "color",
                         col = col(200),
                         type = "upper",
                         order = "hclust",
                         addCoef.col = "black", # Add coefficient of correlation
                         tl.col = "black",
                         tl.srt = 45, #Text label color and rotation
                         # Combine with significance
                         p.mat = p.mat,
                         sig.level = 0.01,
                         insig = "blank",
                         # hide correlation coefficient on the principal diagonal
                         diag = FALSE
      )
    }

    if ("crossout" %in% corrplot){
      #with significance
      corrplot::corrplot(c,
                         type="upper",
                         order="hclust",
                         p.mat = p.mat,
                         sig.level = 0.01)
    }

    if ("blank" %in% corrplot){
      # Leave blank on no significant coefficient
      corrplot::corrplot(c,
                         type="upper",
                         order="hclust",
                         p.mat = p.mat,
                         sig.level = 0.01,
                         insig = "blank")
    }
  }

# Now with a variable for sensor
  corlist <- list()
  sensor_names <- names(data)[c(1:length(resp))]

# thanks to lares maintainer Bernardo Lares <laresbernardo@gmail.com>
# for supporting this part on StackOverflow

  if(length(resp) == 1){
    var_name <- sensor_names[1]
    var_sym <- rlang::sym(var_name)  # Construct symbol from string
    corlist[[1]] <- lares::corr_var(
      df = data,
      var = !!var_sym,
      max_pvalue = 0.05,
      top = 50)
  } else {
    for (i in 1:length(resp)){
      var_name <- sensor_names[i]
      var_sym <- rlang::sym(var_name)  # Construct symbol from string
      corlist[[i]] <- lares::corr_var(
        df = data,
        var = !!var_sym,
        max_pvalue = 0.05,
        ignore = names(data)[c(1:length(resp)[-i])],
        top = 50
      )
    }
  } # end if-conditions

  for (i in 1:length(corlist)){
    df <- corlist[[i]]$data
    var <- df[df$corr <= 0, ]
    utils::write.csv(
            var,
            file.path(
              envrmt$path_statistics,
              paste0(
                sensor_names[i],
                "_delect.csv"
                )
              )
            )
  } # end corlist-loop

print(
  paste0(
    "Calculated autocorrelations for these sensors [",
    sensor_names,
    "]."
    )
  )
saveRDS(
  sensor_names,
  file.path(
    envrmt$path_tmp,
    "sensor_names.rds"
    )
  )
} # end function
