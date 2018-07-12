# Romain GUEDON
# utils.R

#' Load built-in methods
#'
#' @param method the name of the built-in method to be used
#' @return a method list
#' @export
GetMethodInfo <- function(method = NULL){
   load(system.file("methods", "methods.RData", package = "subtypr"))# load an object called methods.list with all methods
   if (!is.null(method)){
      keepers <- which(method == names(methods.list))
      methods.list <- methods.list[[keepers]]
   }
   if (length(methods.list) == 0){
      stop("This method is not pre-implemented in subtypr, be careful on your typo")
   }
   methods.list
}




#' Load built-in metrics
#'
#' @param metric
#'
#' @return a metric list
#' @export
GetMetricInfo <- function(metric = NULL) {
   load(system.file("metrics", "metrics.RData", package = "subtypr"))# load an object called metrics.list with all metrics
   if (!is.null(metric)){
      keepers <- which(metric == names(metrics.list))
      metrics.list <- metrics.list[[keepers]]
   }
   if (length(metrics.list) == 0) {
      stop("This metric is not pre-implemented in subtypr, be careful on your typo")
   }
   metrics.list
}




