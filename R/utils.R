# Romain GUEDON
# utils.R

#' Load built-in methods
#'
#' Load the selected built-in method as a 'method list' to be used with
#'   Tuning function or directly by the user using $Func.
#'   Use names(get_method()) to have a list of the built-in methods.
#'
#'
#' @param method the name of the built-in method to be used.
#' @param list_format logical, to return the result as a list
#'
#' @return a 'method list'. $name is a character with the name to access the
#'   function with the get_method function,
#'   $label is the complete name of the function,
#'   $Func is the function corresponding to the method: it returns a
#'   partition of the samples and some data to use
#'   internal metrics to validate the partition.
#'
#' @export
get_method <- function(method = NULL, list_format = FALSE) {
  # load an object called methods_list with all methods:
  load(system.file("methods", "methods.RData", package = "subtypr"))
  if (!is.null(method)) {
    keepers <- which(names(methods_list) %in% method)
    methods_list <- methods_list[keepers]
    if (!list_format) { # to return the object and not a list of one object
      methods_list <- methods_list[[1]]
    }
  }
  if (length(methods_list) == 0) {
    stop("This method is not pre-implemented in subtypr,
         be careful on your typo")
  }

  methods_list
}




#' Load built-in metrics
#'
#' Load the selected built-in metric as a 'metric-list' to be used in Tuning
#'   function or directly by the user using $Metric.
#'   Use names(get_metric()) to have a list of all the built-in metrics.
#'
#' @param metric a character. The metric to be used.
#' @param list_format logical, to return the result as a list
#'
#' @return a metric list. $name is the name used to access the metric
#'   with get_metric function.
#'   $label is the full name of the metric.
#'   $Metric is either an internal metric or external metric function and gives
#'   a score to the given partition (and some given data if
#'   it's an internal metric).
#'   $maximize is a logical value indicating if the score returned is best when
#'   maximized (TRUE) or when minimized (FALSE).
#' @export
get_metric <- function(metric = NULL, list_format = FALSE) {

  # load an object called metrics_list with all metrics
  load(system.file("metrics", "metrics.RData", package = "subtypr"))

  # keep only the user list
  if (!is.null(metric)) {
    keepers <- which(names(metrics_list) %in% metric)
    metrics_list <- metrics_list[keepers]
    if (length(metrics_list) == 0) {
      stop("This metric is not pre-implemented in subtypr,
           be careful on your typo")
    } else {
      if (!list_format) { # to return the object and not a list of one object
        metrics_list <- metrics_list[[1]]
      }
    }
  } else {
    metrics_list
  }
}


# some syntactic sugar for brevity
at_assert <- assertthat::assert_that


