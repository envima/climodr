#' Autocorrelation
#'
#' Test Data on autocorrelation to produce reliable models
#'
#' @param eval_vec
#'
#' @return
#' @seealso
#'
#' @name autocorr
#' @export autocorr
#'
#' @examples
#'

autocorr <- function(
    method = "monthly",
    pred,
    resp,
    plot.corrplot = TRUE)
  {
  #get data from PreProcessing
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

  #subset data
  data <- data_o[ ,c(pred, resp)]

  #duplicated data
  #data_d <- data[duplicated(data),];
  data <- data[complete.cases(data), ]

  c <- stats::cor(data)

  ### -- smol function from stack overflow for cor-test -- ##
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  };

  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(data);
  head(p.mat[, 1:5]);

  if (plot.corrplot == TRUE){
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"));
    corrplot::corrplot(c,
                       method="color",
                       col=col(200),
                       type="upper",
                       order="hclust",
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black",
                       tl.srt=45, #Text label color and rotation

                       # Combine with significance
                       p.mat = p.mat, sig.level = 0.01, insig = "blank",

                       # hide correlation coefficient on the principal diagonal
                       diag=FALSE
    );

    #with significance
    corrplot::corrplot(c,
                       type="upper",
                       order="hclust",
                       p.mat = p.mat,
                       sig.level = 0.01);

    # Leave blank on no significant coefficient
    corrplot::corrplot(c,
                       type="upper",
                       order="hclust",
                       p.mat = p.mat,
                       sig.level = 0.01,
                       insig = "blank");

    corrplot::corrplot(c,
                       method="number",
                       type = "lower");

    corrplot::corrplot(c,
                       type  = "lower",
                       method="color",
                       addCoef.col="black",
                       order = "AOE",
                       number.cex=0.75);
  }

# Now with a variable for sensor
  corlist <- list()
  sensor_names <- names(data)[c(1:length(pred))]

# thanks to lares maintainer Bernardo Lares <laresbernardo@gmail.com>
# for supporting this part on StackOverflow

  if(length(pred) == 1){
    var_name <- sensor_names[1]
    var_sym <- rlang::sym(var_name)  # Construct symbol from string
    corlist[[1]] <- lares::corr_var(
      df = data,
      var = !!var_sym,
      max_pvalue = 0.05,
      top = 50)
  } else {
    for (i in 1:length(pred)){
      var_name <- sensor_names[i]
      var_sym <- rlang::sym(var_name)  # Construct symbol from string
      corlist[[i]] <- lares::corr_var(
        df = data,
        var = !!var_sym,
        max_pvalue = 0.05,
        ignore = names(data)[c(1:length(pred)[-i])],
        top = 50
      )
    }
  } # end if-conditions

  for (i in 1:length(corlist)){
    df <- corlist[[i]]$data
    var <- df[df$corr <= 0, ]
    write.csv(
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
