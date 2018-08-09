#' Simple quality control on sample-feature matrices
#'
#' Many methods have restrictions on the data they can use (most notably
#' missing data). This is a simple function for assessing the quality of
#' input data. By default it looks for missing (NA) data although the
#' the trigger condition can be modified.
#'
#' @param m a sample-feature matrix
#' @param flag_fxn a boolean function that detects problematic data
#'
#' @return a summary of how the matrix is affected by problematic data
#' @export
#'
flag_data <- function(m, flag_fxn = is.na) {
  # XXX: should this return a table like summary or pca?
  ## Preconditions:
  at_assert(is.function(flag_fxn))

  ## Main:
  cols_with_flagged_data <- unname(apply(m, MARGIN = 2, function(x) sum(flag_fxn(x))))
  rows_with_flagged_data <- unname(apply(m, MARGIN = 1, function(x) sum(flag_fxn(x))))

  flagged_data_cnt <- sum(rows_with_flagged_data)
  at_assert(flagged_data_cnt == sum(cols_with_flagged_data))

  data_less_flagged <- subset (m, !rows_with_flagged_data, !cols_with_flagged_data)

  ## Postconditions & return:
  return(
    list (
      flagged_data_cnt = flagged_data_cnt,
      flagged_rows_cnt = sum (as.logical (rows_with_flagged_data)),
      flagged_cols_cnt = sum (as.logical (cols_with_flagged_data)),
      dims_without_flagged_data = dim (data_less_flagged)
    )
  )
}


#' Reduce a sample-featyure matrix to suitable data
#'
#' @param m a sample-feature matrix
#' @param flag_fxn a boolean function that detects problematic data
#'
#' @return the matrix with all rows and columns that contain problematic data removed
#' @export
#'
#' @examples
#'
filter_flagged_data <- function(m, flag_fxn = is.na) {
  # XXX: or should this actually be in the caret data transformation stuff?
  # XXX: combine with function above?
  # TODO: does this work if it reduces the matrix to nothing

  ## Preconditions:
  at_assert (is.function(flag_fxn))

  ## Main:
  cols_with_flagged_data <- unname(apply(m, MARGIN = 2, function(x) sum(flag_fxn(x))))
  rows_with_flagged_data <- unname(apply(m, MARGIN = 1, function(x) sum(flag_fxn(x))))

  ## Postconditions & return:
  return(
    subset (m, !rows_with_flagged_data, !cols_with_flagged_data)
  )
}


# TODO: also check in multi-omics that each matrix has same samples
# TODO: transform flagged data
# TODO: split negative matrix

