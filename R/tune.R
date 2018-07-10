# Tune (search) parameter space for best fit by various methods

#' Systematically search parameter space for best fit
#'
#' @param tune_fxn
#' @param params
#' @param maximize
#' @param seed
#' @param cores
#'
#' @return
#' @export
#'
#' @examples
tune_exhaustively <- function (
   tune_fxn,
   params,
   maximize,
   seed=NULL,
   cores=NULL
) {
   ## Preconditions & preparation:
   # set random seed if needed
   if (is.null (seed)) { seed <- sample.int (.Machine$integer.max, 1L) }
   # if maximize is string, assume it's an attribute on function returns
   if (is.character (maximize)) { maximize <- function (r) { attr (r, maximize, exact=T) } }


   ## Main:


}


#' Tune parameters by Latin Hypercube sampling
#'
#' NOTE: how do we divide up space?
#'
#' @param tune_fxn
#' @param params
#' @param maximize
#' @param seed
#' @param cores
#'
#' @return
#' @export
#'
#' @examples
tune_lhc <- function (
   tune_fxn,
   params,
   maximize,
   seed=NULL,
   cores=NULL
) {
   ## Preconditions & preparation:
   # set random seed if needed
   if (is.null (seed)) { seed <- sample.int (.Machine$integer.max, 1L) }
   # if maximize is string, assume it's an attribute on function returns
   if (is.character (maximize)) { maximize <- function (r) { attr (r, maximize, exact=T) } }


   ## Main:


}

