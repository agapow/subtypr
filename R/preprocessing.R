# Pre-processing of the data

#' Pre-process the features of a given feature matrix.
#'
#' Use the \code{\link[caret]{preProcess}} function of the package caret to preprocess the data.
#'   Centering and scaling it by default.
#'
#' @param data a feature matrix: samples in rows, features in columns.
#' @param method a character vector specifying the type of processing.
#'   Possible values are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range",
#'   "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign", "corr", "zv", "nzv",
#'   and "conditionalX" (see Details in \code{\link[caret]{preProcess}}).
#' @param ... additional arguments that will be passed to \code{\link[caret]{preProcess}}.
#'
#' @return the preprocessed data.
#' @export
PreProcess <- function(data, method = c("center", "scale"), ...) {

  # Just wrap the caret functions preProcess and predict

  preProcessx <- caret::preProcess(x = data, method = method, ...)


  predictx <- stats::predict(preProcessx, newdata = data)
  predictx
}



library(subtypr)


