# Romain GUEDON
# utils.R

GetMethodInfo <- function(method = NULL){
  load(system.file("methods", "methods.RData", package = "subtypr"))# load an object called methods.list with all methods
  if (!is.null(method)){
    keepers <- which(method == names(methods.list))
    methods.list <- methods.list[keepers]
  } 
  if (length(methods.list) == 0){
    stop("This method is not pre-implemented in subtypr, be careful on your typo")
  }
}


GetMetricInfo <- function(metric = NULL){
  load(system.file("metrics", "metrics.RData", package = "subtypr"))# load an object called metrics.list with all metrics
  if (!is.null(metric)){
    keepers <- which(metric == names(metrics.list))
    metrics.list <- metrics.list[keepers]
  } 
  if (length(metrics.list) == 0){
    stop("This metric is not pre-implemented in subtypr, be careful on your typo")
  }
}
