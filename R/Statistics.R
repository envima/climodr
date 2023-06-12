#' Statistics
#'
#' Calculate Statistics from the predictions
#'
#' @param
#'
#' @return
#' @seealso
#'
#' @name climstats
#' @export climstats
#'
#' @examples
#'
climstats <- function(method = "vif"){
  modlist <- list.files(envrmt$path_models)
  if (method == "vif"){
    vif_stats <- data.frame()
    for (i in 1:length(modlist)){
      mod <- readRDS(file.path(envrmt$path_models, modlist[i]))
      vif <- car::vif(mod)
      vif_stats <- rbind(vif_stats, vif, make.row.names = TRUE)
    } # end for-loop
    return <- vif_stats
  } # end if-condition
 }
