# Preprocessing of the data

#' Preprocess the features of a given feature matrix.
#'
#' Using the `preProcess` func
#'
#' @param data a feature matrix: samples in rows, features in columns.
#' @param method a character vector specifying the type of processing.
#'   Possible values are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range",
#'   "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign", "corr", "zv", "nzv",
#'   and "conditionalX" (see Details of \code{\link[caret]{preProcess}})
#' @param ... additional arguments that will be passed to \code{\link[caret]{preProcess}}
#'
#' @return the preprocessed data.
#' @export
PreProcess <- function(data, method = c("center", "scale"), ...) {

  # Just wrap the caret functions preProcess and predict

  preProcessx <- caret::preProcess(x = data, method = method, ...)


  predictx <- stats::predict(preProcessx, newdata = data)
  predictx

}


