# tuning of methods

evaluation <- function (data.list, method, grid, grid.row, metric = NULL, ground.truth = NULL, return.res = FALSE) {
  full.args <- as.list(base::formals(method$Func))
  full.args$data.list <- data.list
  full.args[names(as.list(grid[grid.row,]))] <- as.list(grid[grid.row,])
  res <- do.call(method$Func, full.args)
  if (return.res) {
     return(res)
  }
  return(metric$Metric(res = res, ground.truth = ground.truth, plot = F))
}


#' Tune the methods to have the best set of parameters
#'
#' tuning considers a list of values of parameters (grid.support) to be tested
#' (all the combinations are tested) and find the set of parameters that have
#' the best value for the metric selected.
#'
#' @param data.list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#' @param method a string being the name of the built-in method to be used or a
#'   'method list' with the same format as the built-in ones.
#' @param grid.support a list with a set of value for each parameter to be
#'   tuned. The typo has to be correct. Use formals(method$Func) to know all the
#'   available parameters.
#' @param metric a string being the name of the built-in metric to be used or a
#'   'metric list' with the same format as the built-in ones. If the chosen
#'   metric is internal, be sure to choose a metric corresponding to the kind of
#'   data returned by the chosen method.
#' @param ground.truth a factor or integer vector being a classification of the
#'   patients. It will be used with external metrics.
#' @param parallel logical, TRUE for parallel computing (default and
#'   recommanded)
#' @param plot logical, set TRUE to plot the metric evaluation of each grid
#'   point to have an overview of the impact of the variation of the parameters
#'   on the value of the considered metric.
#' @param verbose logical, set FALSE to avoid printing of informations
#'
#' @return the result list. $metric.val contain the best value of the metric
#'   among all the grid
#'
#'   $parameters is the corresponding set of parameters
#'
#'   $method.used contains the name of the tuned method, to know where the
#'   result comes from.
#'
#'   $ method.res contains the result of the method, i.e. a partition and the
#'   data returned by the method, to allow external and/or internal validation
#'   by other metrics.
#'
#'
#' @export
tuning <- function(data.list, method, grid.support, metric = "asw.affinity", ground.truth = NULL, parallel = TRUE, plot = FALSE, verbose = TRUE) {

  if (is.character(method)) {method <- get_method(method)}
  if (is.character(metric)) {metric <- get_metric(metric)}

  grid <- base::expand.grid(grid.support, stringsAsFactors = FALSE)
  l <- dim(grid)[1]

  # because sometimes it's too long, we warn the user:
  if (verbose) print(paste0("evaluation of ",l," possibilities..."))

  t1 <- Sys.time()
  # evaluation of every possibilities
  if (parallel) {
    no.cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no.cores, type = "FORK")
    evaluations <- parallel::parLapply(cl, 1:l, function(grid.row) evaluation(data.list, method, grid, grid.row, metric, ground.truth))
    parallel::stopCluster(cl)
  } else {
    evaluations <- lapply(1:l, function(grid.row) evaluation(data.list, method, grid, grid.row, metric, ground.truth))
  }
  if (plot) {
    plot(x = 1:l, y = evaluations, xlab = "grid points", ylab = metric$label, main = paste0(metric$label, " for all grid points"))
  }

  # time of execution
  if (verbose) print(paste0("... done in ", as.character(round(Sys.time() - t1, 2)), " seconds"))
  if (verbose) print("Find the best parameters...")

  # find the best
  if (metric$maximize) {
    max.ind <- max.col(t(evaluations))
  } else {#minimize x ~ maximize -x
    max.ind <- max.col(-t(evaluations))
  }

  if (verbose) print("... done !")

  # Reconstruct the result
  res <- evaluation(data.list, method, grid, max.ind, metric, return.res = TRUE)

  return(list(metric.val  = evaluations[[max.ind]],
              parameters  = as.list(grid[max.ind, ]),
              method.used = method$name,
              method.res  = res))
}


#' An overview of the results with all available metrics
#'
#' overview_metrics use the result of the tuning and diagnoses it using a list of
#' selected metric. It's useful to see how other metrics evaluate the result
#' tuned with only one given metric which is subject to biases.
#'
#' @param method.result a list returned by built-in methods i.e. a list with an
#'   element `partition` and `data.returned`. To use directly the result of the
#'   function tuning, use the element $method.res of the result of the tuning.
#' @param internal.metrics a character vector indicating the name of internal
#'   metrics to be used. Be careful, the internal metric used must correspond to
#'   the data returned by the selected method (a distance matrix? an affinity
#'   matrix?, a feature matrix? ...)
#' @param ground.truth factor or integer vector being a classification of the
#'   patients to be used with external metrics.
#' @param print logical. Print or not the values of metrics in the console.
#' @param plot logical. Plot or not silhouette graph
#'
#' @param external.metrics a character vector with the name of external metrics
#'   to be used with the provided ground-truth. If NULL, all the external
#'   metrics are evaluated.
#'
#' @return a data.frame with the value of metrics.
#' @export
#'
overview_metrics <- function(method.result,
                         internal.metrics = NULL,
                         ground.truth  = NULL,
                         external.metrics = NULL,
                         print         = T,
                         plot          = T) {


  all.metrics <- get_metric(extract = FALSE)
  internal.metrics.list <- NULL
  external.metrics.list <- NULL

  # First select internal.metrics:
  if (!is.null(internal.metrics)) {
    internal.metrics.list <- all.metrics[names(all.metrics) %in% internal.metrics]
  }

  # Then select external.metrics
  if (!is.null(external.metrics)) {
    external.metrics.list <- all.metrics[names(all.metrics) %in% external.metrics]
  } else {
    # otherwise, if a ground-truth is provided, it selects all external metrics.
    if (!is.null(ground.truth)) {
      mask <- rep(FALSE, length(all.metrics))
      i <- 1
      for (metric in all.metrics) {
        if (!metric$internal){
          mask[i] <- TRUE
        }
        i <- i + 1
      }
      external.metrics.list <- all.metrics[mask]
    }
  }
  #concatenate the list
  actual.list <- do.call(c, list(internal.metrics.list, external.metrics.list))
  l <- length(actual.list)
  if (l == 0) {
    stop("Neither internal metric nor ground-truth provided, please provide a ground-truth (and/) or a list of internal metrics" )
  } else {
    #compute the values of metrics in actual.list
    metrics.values <- data.frame(values = 1:l, row.names = names(actual.list))
    for (metric in actual.list) {
      metrics.values[metric$name, "values"] <- metric$Metric(method.result, ground.truth, plot)
      if (print) print(paste0(metric$label, " = ", metrics.values[metric$name, "values"]))
    }
    return(metrics.values)
  }
}


