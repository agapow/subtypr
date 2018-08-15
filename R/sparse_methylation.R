#' Sparse data to test tools
#'
#' This is the methylation data from `gbm_data`, see `?gbm_data` but with
#'   missing values. We removed a variable percentage of features per sample.
#'   This variable percentage was generated based on a gaussian distribution
#'   centered on 0.12, with a standard deviation of 0.08. We set 0 for the
#'   negative values.
#'
#' @format It's a samples x feature matrix.
#'
"sparse_methylation"

# library(subtypr)
#
#
# sparse_data <- list(
#   data_list = gbm_data$data_list,
#   survival = gbm_data$survival
# )
#
#
# create_sparse_data <- function(data_list, print=F) {
#   ## Preconditions & preparation
#   n_samples <- nrow(data_list[[1]])
#   n_layers <- length(data_list)
#   sparse_data_list <- as.list(data_list)
#
#   ## Main
#   for (x in 1:n_layers) {
#
#     n_features <- ncol(sparse_data_list[[x]])
#
#     # Number of missing features per samples:
#     n_missing_features <- as.matrix(
#       round(n_features*rnorm(n_samples, 0.12, 0.08))
#       )
#     n_missing_features[n_missing_features < 0] <- 0
#
#     # Index of missing features per samples:
#     missing_features <- apply(
#       n_missing_features,
#       1,
#       function(n) sample(n_features, n)
#     )
#     for (i in 1:n_samples) {
#       sparse_data_list[[x]][i, missing_features[[i]]] <- NA
#       # Check missing rate per sample:
#       if (print) {
#         print(paste0(
#         "Missing rate for patient ", i, ": ",
#         mean(is.na(sparse_data_list[[x]][i,]))
#         ))
#       }
#     }
#   }
#   sparse_data_list
# }
#
# ww <- create_sparse_data(sparse_data$data_list)
#
# sparse_methylation <- ww$methylation
#
# save(sparse_methylation, file="./data/sparse_methylation.RData")
