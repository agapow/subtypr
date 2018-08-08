# Features selection functions

#' Variance cutoff
#'
#' @param data A feature matrix, rows are samples and columns are features.
#' @param cutoff Minimum variance to be kept.
#'
#' @return data with only selected features.
#'
select_variance <- function(data, cutoff) {
  features_variances <- apply(data, 2, function(x) var(x, na.rm = TRUE))
  index_features_ketp <- which(features_variances > cutoff)
  data[, index_features_ketp]
}

#' Apply principal components analysis
#'
#' @param data A feature matrix, rows are samples and columns are features.
#' @param min_pve Minimum of percentage of variance explained by the selected
#'   principal components.
#'
#' @param scale A logical value indicating wether the data is normalized before PCA.
#'
#' @return A features matrix with only selected principal components.
#'
select_pca <- function(data, min_pve = 1, scale = TRUE) {
  prcomps = prcomp(data, scale. = scale)
  vars <- apply(prcomps$x, 2, var)
  props <- vars/sum(vars)
  pves = as.vector(cumsum(props))
  num = which(pves > min_pve)[1]
  coeff = prcomps$ro
  res = (as.matrix(data) %*% coeff)[, 1:num]
  as.data.frame(res)
}

#' Mean Absolute Deviation (MAD) cutoff
#'
#' @param data A feature matrix, rows are samples and columns are features.
#' @param cutoff Minimum MAD to be kept.
#'
#' @return data with only selected features.
#'
select_mad <- function(data, cutoff) {
  features_mads <- apply(data, 2, function(x) mad(x, na.rm = TRUE))
  index_features_ketp <- which(features_mads > cutoff)
  data[, index_features_ketp]
}

#' Selection of important features
#'
#' The parameters for each methods:
#'
#'   * "variance": `cutoff`: Minimum variance to be kept.
#'   * "pca": `min_pve`: Minimum of percentage of variance explained by the
#'     selected principal components.
#'   * "mad": `cutoff`: Minimum MAD to be kept.
#'
#' @param data a feature matrix, rows are samples and columns are features
#' @param method "variance", "pca",
#' @param ... args to be passed to the selected method:
#'   * "variance": `cutoff`: Minimum variance to be kept.
#'
#'   * "pca":
#'     * `min_pve`: Minimum of percentage of variance explained by the
#'     selected principal components.
#'     * `scale`: A logical value indicating wether the data is normalized
#'     before PCA.
#'
#'   * "mad": `cutoff`: Minimum MAD to be kept.
#'
#' @return data with only selected features or principal components.
#' @export
#'
select_features <- function(data, method = c("variance", "pca", "mad"), ...) {

  method <- match.arg(method)

  if (method == "variance") {
    select_variance(data, ...)
  } else if (method == "pca") {
    select_pca(data, ...)
  } else if (method == "mad") {
    select_mad(data, ...)
  }
}


