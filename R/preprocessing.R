# Pre-process feature matrices before analysis.
#'
#' Often, data must be adjusted (scaled, imputed, etc.) before it can be
#' analysed.  This method presents a simple and consistent interface for the same.
#'
#' Internally this is a wrapper of the equivalent methods from the
#' \link[caret]{caret}
#' package, that simplifies and streamlines usage. Note that while it suits
#' our purposes to have a single step to adjust the data, caret package
#' presents this as two steps - assess preprocessing and then carry it out.
#' This is because you may need to calculate preprocessing over the training
#' data and then transform the training & test data in the same way, or where
#' you wish to explicitly capture the preprocessing stage (e.g. PCA).
#'
#' @param m a feature matrix with samples in rows and features in columns.
#' @param method list of preprocessing methods as defined in caret::preProcess,
#'    centering and scaling it by default. See \code{\link[caret]{preProcess}}.
#' @param ... preprocessing methods parameters as required by caret::preProcess
#'
#' @return the preprocessed data.
#' @export
#'
#' @examples
#'
PreProcess <- function(data, method = c("center", "scale"), ...) {

  # Just wrap the caret functions preProcess and predict

  preProcessx <- caret::preProcess(x = data, method = method, ...)


  predictx <- stats::predict(preProcessx, newdata = data)
  predictx
}

# NOTE: caret is not calling stats::predict but it's own predict.preProcess.

# XXX: would it be useful to have our own versions of the two-step transform
# XXX: how do you write other transformation functions?



