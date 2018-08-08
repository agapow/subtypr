# tuning of methods

#' Evaluate parameters
#'
#' Evaluate a set of parameters for a given method with a given metric.
#'
#'
#' @param data_list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#'
#' @param method a string being the name of the built-in method to be used or a
#'   'method list' with the same format as the built-in ones.
#'
#' @param grid the grid generated in `tuning()`.
#'
#' @param metric a string being the name of the built-in metric to be used or a
#'   'metric list' with the same format as the built-in ones. If the chosen
#'   metric is internal, be sure to choose a metric corresponding to the kind of
#'   data returned by the chosen method.
#'
#' @param true_partition a factor or integer vector being a classification of the
#'   patients. It will be used with external metrics.
#'
#' @param grid_row the row indicating the set of parameters to be tested in the grid

#' @param return_metric logical. To return the result of the method instead of the value of the metric.
#'
#' @return the value of the metric or the result of the method if `return_metric` is TRUE
#'
evaluation <- function(data_list, method, grid, grid_row,
                       metric = NULL,
                       true_partition = NULL,
                       return_metric = TRUE) {

  ## Preconditions & preparation:
  # get all the args from the method:
  full_args <- as.list(base::formals(method))

  # add the data: (not in the grid because of it's size):
  full_args$data_list <- data_list

  # add the args of the grid:
  full_args[colnames(grid)] <- as.list(grid[grid_row, ])

  if (return_metric){
    full_args$minimal_return <- TRUE
  }
  ## Main
  res <- do.call(method, full_args)

  ## Postconditions & return

  if (!return_metric) {
    res
  } else {
    metric$metric(
      pred_partition = res$partition,
      true_partition = true_partition,
      data_for_metric = res[[res$element_for_metric]]
    )
  }
}


#' Tunes the methods to have the best set of parameters
#'
#' tuning considers a list of values of parameters (grid_support) to be tested
#' (all the combinations are tested) and find the set of parameters that have
#' the best value for the metric selected.
#'
#' @param data_list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#'
#' @param method a string being the name of the built-in method to be used or a
#'   'method list' with the same format as the built-in ones.
#'
#' @param grid_support a list with a set of value for each parameter to be
#'   tuned. The typo has to be correct. Use formals(method$func) to know all the
#'   available parameters.
#'
#' @param metric a string being the name of the built-in metric to be used or a
#'   'metric list' with the same format as the built-in ones. If the chosen
#'   metric is internal, be sure to choose a metric corresponding to the kind of
#'   data returned by the chosen method.
#'
#' @param true_partition a factor or integer vector being a classification of the
#'   patients. It will be used with external metrics.
#'
#' @param parallel logical, TRUE for parallel computing (default and
#'   recommanded).
#'
#' @param plot logical, set TRUE to plot the metric evaluation of each grid
#'   point to have an overview of the impact of the variation of the parameters
#'   on the value of the considered metric.
#'
#' @param verbose logical, set FALSE to avoid printing of informations
#' @param save_results logical, to save the result of tuning in `path_to_file`
#' @param file_name filename finishing by .RData or .rda
#' @param path_to_file if save_results, results of the tuning are save to this
#'   path
#' @param ncores integer, the number of core to be used for the parallel
#'   computation.
#'   If NULL, it takes the maximum number of cores - 1, using the function
#'   \code{\link[parallel]{detectCores}}.

#'
#' @return a list:
#'
#'  * $metric_val contain the best value of the metric
#'    among all the grid
#'
#'  * $parameters is the corresponding set of parameters
#'
#'  * $method_used contains the name of the tuned method, to know where the
#'   result comes from.
#'
#'  * $method_res contains the result of the method, i.e. a partition and the
#'   data returned by the method, to allow external and/or internal validation
#'   by other metrics.
#'
#'
#' @export
tuning <- function(data_list, method, grid_support, metric,
                   true_partition = NULL,
                   parallel = TRUE,
                   ncores = NULL,
                   plot = FALSE,
                   verbose = TRUE,
                   save_results = FALSE,
                   file_name = "tuning_result.RData",
                   path_to_file = "./") {

  ## Preconditions & preparation:
  if (is.character(method)) {
    method <- get_method(method)
  }
  if (is.character(metric)) {
    metric <- get_metric(metric)
  }

  grid <- base::expand.grid(grid_support, stringsAsFactors = FALSE)
  l <- dim(grid)[1]

  ## Main

  # because sometimes it's too long, we warn the user:
  if (verbose) print(paste0("Evaluation of ", l, " possibilities..."))


  # evaluation of every possibilities:

  if (parallel) {
    # cluster setup
    if (is.null(ncores)) ncores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(ncores, type = "FORK")

    # computation
    execution_time <- system.time(evaluations <- parallel::parLapply(
      cl = cl,
      X = 1:l,
      fun = function(grid_row) evaluation(
          data_list = data_list,
          method = method,
          grid = grid,
          grid_row = grid_row,
          metric = metric,
          true_partition = true_partition,
          return_metric = TRUE
        )
    ))


    parallel::stopCluster(cl)
  } else {
    execution_time <- system.time(
      evaluations <- lapply(1:l, function(grid_row) evaluation(
        data_list = data_list,
        method = method,
        grid = grid,
        grid_row = grid_row,
        metric = metric,
        true_partition = true_partition,
        return_metric = TRUE
        ))
    )
  }
  if (verbose) {
    print(paste0(
      "... done in ", round(execution_time[3], 3),
      " seconds", "!"
    ))
  }

  ## Postconditions & return

  # Find the best set of parameters

  if (verbose) print("Find the best parameters...")

  if (metric$maximize) {
    max_ind <- max.col(t(evaluations))
  } else { # Minimizing x is equivalent to maximizing -x
    max_ind <- max.col(-t(evaluations))
  }

  if (verbose) print("... done !")

  if (plot) {
    plot(
      x = 1:l, y = evaluations,
      xlab = "grid points", ylab = metric$label,
      main = paste0(metric$label, " for all grid points")
    )
  }


  # Reconstruct the result:
  res <- evaluation(data_list, method, grid, max_ind, metric, return_res = TRUE)

  # Save the computation:

  if (save_results) {
    while (!file.exists(path_to_file)) {
      path_to_file <- readline("Please provide a valid path
                               for saving the file: ")
    }
    full_file_name <- paste0(path_to_file, file_name)
    if (file.exists(full_file_name)) {
      full_file_name <- paste0(path_to_file, "new_", file_name)
    }
    tuning_result <- list(
      date_of_execution = Sys.Date(),
      time_of_execution = execution_time,
      grid_support_provided = grid_support,
      metric_used = metric,
      list_of_metrics_values = t(evaluations),
      tuning_result_list = list(
        metric_val = evaluations[[max_ind]],
        parameters = as.list(grid[max_ind, ]),
        method_used = method,
        method_res = res
      )
    )
    print(full_file_name)
    save(tuning_result, file = full_file_name)
  }

  # Return:
  return(list(
    metric_val = evaluations[[max_ind]],
    parameters = as.list(grid[max_ind, ]),
    method_used = method,
    method_res = res
  ))
}


#' An overview of the results with all available metrics
#'
#' overview_metrics use the result of the tuning and diagnoses it using a list
#'   of selected metric. It's useful to see how other metrics evaluate the
#'   result tuned with only one given metric which is subject to biases.
#'
#' @param method_result a list returned by built-in methods i.e. a list with an
#'   element `partition` and `data_returned`. To use directly the result of the
#'   function tuning, use the element $method_res of the result of the tuning.
#' @param internal_metrics a character vector indicating the name of internal
#'   metrics to be used. Be careful, the internal metric used must correspond to
#'   the data returned by the selected method (a distance matrix? an affinity
#'   matrix?, a feature matrix? ...)
#' @param true_partition factor or integer vector being a classification of the
#'   patients to be used with external metrics.
#' @param print logical. Print or not the values of metrics in the console.
#' @param plot logical. Plot or not silhouette graph
#'
#' @param external_metrics a character vector with the name of external metrics
#'   to be used with the provided ground-truth. If NULL, all the external
#'   metrics are evaluated.
#'
#' @return a data.frame with the value of metrics.
#' @export
#'
overview_metrics <- function(method_result,
                             internal_metrics = NULL,
                             true_partition = NULL,
                             external_metrics = NULL,
                             print = T,
                             plot = T) {
  all_metrics <- get_metric(list_format = TRUE)
  internal_metrics_list <- NULL
  external_metrics_list <- NULL

  # First select internal_metrics:
  if (!is.null(internal_metrics)) {
    internal_metrics_list <- all_metrics[names(all_metrics)
    %in%
      internal_metrics]
  }

  # Then select external_metrics
  if (!is.null(external_metrics)) {
    external_metrics_list <- all_metrics[names(all_metrics)
    %in%
      external_metrics]
  } else {
    # otherwise, if a ground-truth is provided, it selects all external metrics.
    if (!is.null(true_partition)) {
      mask <- rep(FALSE, length(all_metrics))
      i <- 1
      for (metric in all_metrics) {
        if (!metric$internal) {
          mask[i] <- TRUE
        }
        i <- i + 1
      }
      external_metrics_list <- all_metrics[mask]
    }
  }
  # concatenate the list
  actual_list <- do.call(c, list(internal_metrics_list, external_metrics_list))
  l <- length(actual_list)
  if (l == 0) {
    stop("Neither internal metric nor ground-truth provided,
         please provide a ground-truth (and/) or a list of internal metrics")
  } else {
    # compute the values of metrics in actual_list
    metrics_values <- data.frame(values = 1:l, row.names = names(actual_list))
    for (metric in actual_list) {
      metrics_values[metric$name, "values"] <- metric$metric(
        method_result,
        true_partition, plot
      )
      if (print) {
        print(paste0(
          metric$label, " = ",
          metrics_values[metric$name, "values"]
        ))
      }
    }
    return(metrics_values)
  }
}
