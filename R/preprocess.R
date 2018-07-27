# preprocess data
# just a wrapper of the caret functions mainly
# note that this preprocesses in a single step

#' Preprocess and prepare data before analysis
#'
#' Often, data must be adjusted (scaled, imputed, etc.) before it can be
#' analysed.  This method presents a simple and consistent interface for the same.
#'
#' Internally this is a wrapper of the equivalent methods from the caret
#' package, that simplifies and streamlines usage. Note that while it suits
#' our purposes to have a single step to adjust the data, caret package
#' presents this as two steps - assess preprocessing and then carry it out.
#' This is because you may need to calculate preprocessing over the training
#' data and then transform the training & test data in the same way, or where
#' you wish to explicitly capture the preprocessing stage (e.g. PCA).
#'
#' @param m matrix or dataframe
#' @param method list of preprocessing methods as defined in caret::preProcess
#' @param ... preprocessing methods parameters as required by caret::preProcess
#'
#' @return transformed data
#' @export
#'
#' @examples
#'
transform <- function (m, method, ...) {
   # method_params <- list (...)
   preproc <- caret::preProcess (m, ...)
   return (caret::predict (preproc, m))
}

