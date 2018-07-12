




Evaluation <- function(data.list, method, grid, grid.row, metric, ground.truth = NULL) {
  full.args <- as.list(grid[grid.row,])
  full.args$data.list <- data.list
  res <- do.call(method$Func, full.args)
  return(metric$Metric(res = res, ground.truth = ground.truth, plot = F))

}


Tuning <- function(data.list, method, grid.support, metric, ground.truth = NULL, parallel = TRUE, plot = FALSE, quiet = FALSE) {
  # compute the grid
  grid <- base::expand.grid(grid.support)
  l <- dim(grid)[1]

  # because sometimes it's too long, we warn the user:
  if (!quiet) print(paste0("Evaluation of ",l," possibilities..."))

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

  #time of execution
  if (!quiet) print(paste0("... done in ", as.character(round(Sys.time() - t1, 2)), " seconds"))
  if (!quiet) print("Find the best parameters...")

  #find the best
  if (metric$maximize) {
    max.ind <- max.col(t(evaluations))
  } else {#minimize x ~ maximize -x
    max.ind <- max.col(-t(evaluations))
  }

  if (!quiet) print("... done !")

  return(list(best.value = evaluations[[max.ind]],
              parameters = as.list(grid[max.ind, ])))
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
