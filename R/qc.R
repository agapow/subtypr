#' Simple quality control on sample-feature matrices
#'
#' Many methods have restrictions on the data they can use (most notably
#' missing data). This is a simple function for assessing the quality of
#' input data, which allows the trigger condition to be modified.
#'
#' @param m a feature matrix with samples in rows and features in columns
#' @param flag_fxn a boolean function that detects problematic data
#'
#' @return a summary of how the matrix is affected by problematic data
#' @export
#'
flag_data <- function (m, flag_fxn=is.na) {
  # XXX: shouldthis return a table like summary or pca?
  ## Preconditions:
  assert_that (is.function (flag_fxn))

  ## Main:
  cols_with_flagged_data <- unname (apply (m, MARGIN = 2, function(x) sum (flag_fxn (x))))
  rows_with_flagged_data <- unname (apply (m, MARGIN = 1, function(x) sum (flag_fxn (x))))
  flagged_data <- sum (rows_with_flagged_data)

  data_less_flagged = m[! rows_with_flagged_data, ! cols_with_flagged_data]

  ## Postconditions & return:
  return (c (
      flagged_data = flagged_data,
      rows_with_flagged_data = unname (rows_with_flagged_data),
      cols_with_flagged_data = unname (cols_with_flagged_data),
      dims_without_flagged_data = dim (m)
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
filter_flagged_data <- function (m, flag_fxn=is.na) {
  # XXX: or should this actually be in the caret data transformation stuff?
  # XXX: combine with function above?
  # TODO: does this work if it reduces the matrix to nothing

  ## Preconditions:
  assert_that (is.function (flag_fxn))

  ## Main:
  cols_with_flagged_data <- unname (apply (m, MARGIN = 2, function(x) sum (flag_fxn (x))))
  rows_with_flagged_data <- unname (apply (m, MARGIN = 1, function(x) sum (flag_fxn (x))))

  ## Postconditions & return:
  return (
    m[! rows_with_flagged_data, ! cols_with_flagged_data]
  )
}


# TODO: also check in multi-omics that each matrix has same samples


