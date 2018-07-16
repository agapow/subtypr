




Evaluation <- function(data.list, method, grid, grid.row, metric = NULL, ground.truth = NULL, return.res = FALSE) {
  full.args <- as.list(base::formals(method$Func))
  full.args$data.list <- data.list
  full.args[names(as.list(grid[grid.row,]))] <- as.list(grid[grid.row,])
  res <- do.call(method$Func, full.args)
  if (return.res) {
     return(res)
  }
  return(metric$Metric(res = res, ground.truth = ground.truth, plot = F))
}


Tuning <- function(data.list, method, grid.support, metric = "asw", ground.truth = NULL, parallel = TRUE, plot = FALSE, verbose = TRUE) {

  if (is.character(method)) method <- GetMethod(method)
  if (is.character(metric)) metric <- GetMetric(metric)

  grid <- base::expand.grid(grid.support)
  l <- dim(grid)[1]

  # because sometimes it's too long, we warn the user:
  if (verbose) print(paste0("Evaluation of ",l," possibilities..."))

  t1 <- Sys.time()
  # evaluation of every possibilities
  if (parallel) {
    no.cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no.cores, type = "FORK")
    evaluations <- parallel::parLapply(cl, 1:l, function(grid.row) Evaluation(data.list, method, grid, grid.row, metric, ground.truth))
    parallel::stopCluster(cl)
  } else {
    evaluations <- lapply(1:l, function(grid.row) Evaluation(data.list, method, grid, grid.row, metric, ground.truth))
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
  res <- Evaluation(data.list, method, grid, max.ind, metric, return.res = TRUE)

  return(list(metric.val  = evaluations[[max.ind]],
              parameters  = as.list(grid[max.ind, ]),
              method.used = method$name,
              method.res  = res))
}


MetricValues <- function(tuning.result, metrics.names = NULL, ground.truth = NULL, print = T, plot = T){

  metrics.list <- GetMetric(metric = metrics.names)
  l <- length(metrics.list)
  if (length(metrics.list[[1]]) == 1){# the first element is not a metric list but the first element of the metric list i.e. its name
    metrics.list <- list(metrics.list)
  }
  metrics.values <- data.frame(values = 1:l, row.names = names(metrics.list))
  for (metric in metrics.list) {
    metrics.values[metric$name, "values"] <- metric$Metric(tuning.result$method.res, ground.truth, plot)
    if (print) print(paste0(metric$name, " = ", metrics.values[metric$name, "values"]))
  }
  return(metrics.values)
}









# # GridGenerator creates a list with all the possibilities in a list that are spanned by parameters.list, check the demo.Rmd to see how to set the
# # parameters
# OldGridGenerator <- function(parameters.list, grid, i, current_args) {
#
#     rlen <- length(parameters.list)
#     param.names <- names(parameters.list)
#     tempList <- list()
#     ## last parameter of the list, no need to recursively use looper function
#     if (i == rlen) {
#         for (param in parameters.list[[i]]) {
#             current_args[[param.names[i]]] <- param
#             tempList <- do.call(c, list(tempList, list(current_args)))
#         }
#         grid <- do.call(c, list(grid, tempList))
#     } else {
#         for (param in parameters.list[[i]]) {
#             current_args[[param.names[i]]] <- param
#             grid <- GridGenerator(parameters.list, grid, i + 1, current_args)
#         }
#     }
#     return(grid)
# }
#
#
#
# # for a given list of datasets, it computes the Func with args and evaluate the clusters with the metric
# EvalFunc <- function(datasets, args, Func, metric, returnRes = F) {
#     full.args <- as.list(formals(Func))
#     full.args[["datasets"]] <- datasets  # add data to the args
#     param.names <- names(args)
#
#     # add the tested values to the full.args
#     for (i in 1:length(args)) {
#         full.args[[param.names[i]]] <- args[[i]]
#     }
#     res <- do.call(Func, full.args)
#     if (returnRes) {
#         return(res)
#     }
#     eval <- metric$metric(resOfExecute = res, groundtruth = metric$groundtruth)
#     return(eval)
# }
#
#
# # Tuning is the main function and it returns the list of arguments that had the best value of metric (e.g. NMI) among all the
# # possibilities
#
#
# OldTuning <- function(datasets, parameters.list, Func, metric, plot = F) {
#     # Find all possibilities spanned by parameters.list
#     grid <- list()
#     currentArg <- list()
#     all <- GridGenerator(parameters.list, grid, 1, currentArg)
#     # Initiate cluster
#     no_cores <- parallel::detectCores() - 1
#     cl <- parallel::makeCluster(no_cores, type = "FORK")
#
#     # Export required variables clusterExport(cl,c('EvalFunc','calNMI'))
#
#     # Print the number of possibilities
#     totalCallNumb <- length(all)
#     print(paste0("Test of ", totalCallNumb, " possibilities. It can take a while..."))
#
#     # EvalFunc of all possibilities
#     evals <- parallel::parLapply(cl, all, function(args) EvalFunc(datasets, args, Func, metric))
#     #
#     print("... done !")
#     parallel::stopCluster(cl)
#
#     # plot
#     if (plot) {
#         plot(x = 1:totalCallNumb,
#              y = evals, xlab = "indice of the corresponding set of arguments",
#              ylab = "metric value",
#              main = "Values of the metric for all the tested possibilities")
#     }
#
#     # Choose the best
#     if (metric$maxOrMin == "max") {
#         i_max <- max.col(t(evals))
#     } else {
#         i_max <- max.col(-t(evals))
#     }
#     arg_max <- all[[i_max]]
#     max <- evals[[i_max]]
#     return(list(best.arg = arg_max, metric.value = max))
# }
