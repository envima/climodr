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

autocorr <- function(eval){
  #get data from PreProcessing
  data_o <- read.csv(file.path(envrmt$path_tfinal, "final_monthly.csv"));

  #subset data
  data <- data_o[ ,eval]; #c(6,9,12,13,17:21,24:36,38,39,41,44:57)

  #duplicated data
  #data_d <- data[duplicated(data),];
  data_c <- data[complete.cases(data), ];

  c <- cor(data_c);

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
  p.mat <- cor.mtest(data_c);
  head(p.mat[, 1:5]);

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

  # Now with a variable for sensor
  tem <- lares::corr_var(data_c, Ta_200, max_pvalue = 0.05,
                  ignore = c("rH_200", "SWDR_200","P_RT_NRT"),
                  top = 50);
  pre <- lares::corr_var(data_c, P_RT_NRT, max_pvalue = 0.05,
                  ignore = c("rH_200", "SWDR_200","Ta_200"),
                  top = 50);
  sun <- lares::corr_var(data_c, SWDR_200, max_pvalue = 0.05,
                  ignore = c("rH_200", "Ta_200","P_RT_NRT"),
                  top = 50);
  reh <- lares::corr_var(data_c, rH_200, max_pvalue = 0.05,
                  ignore = c("Ta_200", "SWDR_200","P_RT_NRT"),
                  top = 50);

  # Temperature data frame
  temdf <- tem$data;
  temdfvar <- temdf[temdf$corr<=0,];
  write.csv(temdfvar, file.path(envrmt$path_statistics, "tem_delect.csv"));

  # Pecipitation data frame
  predf <- pre$data;
  predfvar <- predf[predf$corr<=0,];
  write.csv(predfvar, file.path(envrmt$path_statistics, "pre_delect.csv"));

  # solar radiation data frame
  sundf <- sun$data;
  sundfvar <- sundf[sundf$corr<=0,];
  write.csv(sundfvar, file.path(envrmt$path_statistics, "sun_delect.csv"));

  # reh data frame
  rehdf <- reh$data;
  rehdfvar <- rehdf[rehdf$corr<=0,];
  write.csv(rehdfvar,file.path(envrmt$path_statistics, "reh_delect.csv"));


  new <- data_o%>% select(-c(rehdfvar$variables));

  data <- data[rehdfvar$variables,];


    # variance infection factor (vif-factor)

} # end function
