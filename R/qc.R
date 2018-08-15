#' Simple quality control on sample-feature matrices
#'
#' Many methods have restrictions on the data they can use (most notably
#' missing data). This is a simple function for assessing the quality of
#' input data. By default it looks for missing (NA) data although the
#' the trigger condition can be modified.
#'
#' @param m A sample-feature matrix.
#' @param flag_fxn A boolean function that detects problematic data.
#' @param plot logical, to plot the histogram of the percentage of flagged
#'   values per rows or columns.
#' @param print logical, to print informations on the number of flagged elements
#'   in the data.
#' @param filter logical wether to filter the data. The rows (resp. columns)
#'   with a percentage of flagged data greater than `thresh[1]`
#'   (resp. `thresh[2]`) are removed.
#' @param thresh threshold vector: first for rows, second for columns.
#' @return The filtered data if filter is TRUE. Nothing otherwise.
#' @export
#'
flag_data <- function(m, flag_fxn = is.na, filter = FALSE,
                      thresh = c(0.2, 0.2), plot = FALSE, print = TRUE) {
  # XXX: should this return a table like summary or pca?

  ## Preconditions:
  at_assert(is.function(flag_fxn))


  ## Main:
  cols_with_flagged_data <- unname(apply(
    m,
    MARGIN = 2,
    function(x) sum(flag_fxn(x))))
  rows_with_flagged_data <- unname(apply(
    m,
    MARGIN = 1,
    function(x) sum(flag_fxn(x))))

  flagged_data_cnt <- sum(rows_with_flagged_data)
  at_assert(flagged_data_cnt == sum(cols_with_flagged_data))

  data_less_flagged <- subset(m, !rows_with_flagged_data, !cols_with_flagged_data)

  # Percentage of flagged data per row/col:

  flag_prct_cols <- apply(m, MARGIN = 2, function(x) mean(flag_fxn(x)))
  flag_prct_rows <- apply(m, MARGIN = 1, function(x) mean(flag_fxn(x)))

  ## Plot & print
  if (plot) {
    par(mfrow = c(2, 1))
    hist(
      flag_prct_cols,
      xlab = "Flagged percentage",
      ylab = "Count",
      main = "Flagged data percentage of columns")
    hist(
      flag_prct_rows,
      xlab = "Flagged percentage",
      ylab = "Count",
      main = "Flagged data percentage of rows")
    par(mfrow = c(1, 1))
  }
  if (print) {
    print(paste0(
      "Number of flagged elements = ",
      flagged_data_cnt))
    print(paste0(
      "Number of flagged rows = ",
      sum(as.logical(rows_with_flagged_data))))
    print(paste0(
      "Number of flagged columns = ",
      sum(as.logical(cols_with_flagged_data))))
    print(paste0(
      "Dimensions of data without flagged data at all = ", "(",
      dim(data_less_flagged)[1], ",", dim(data_less_flagged)[2], ")"))
  }
  if (filter) {
      keeped_rows <- which(flag_prct_rows < thresh[1])
      keeped_cols <- which(flag_prct_cols < thresh[2])
      new_m <- m[keeped_rows, keeped_cols]
    }
    new_m
}




# filter_flagged_data <- function(flag_list, thresh_rows, thresh_cols) {
#   # XXX: or should this actually be in the caret data transformation stuff?
#   # XXX: combine with function above?
#   keeped_rows <- which(flag_prct_rows < thresh_rows)
#   keeped_cols <- which(flag_prct_cols < thresh_cols)
#   m[keeped_rows, keeped_cols]
# }




# transform_flagged_data <- function(m, flag_fxn = is.na, trans_val) {
#
#   ## Preconditions:
#   at_assert(is.function(flag_fxn))
#
#   ## Main:
#   cols_with_flagged_data <- unname(apply(
#     m, MARGIN = 2, function(x) sum(flag_fxn(x))))
#   rows_with_flagged_data <- unname(apply(
#     m, MARGIN = 1, function(x) sum(flag_fxn(x))))
#
#   ## Postconditions & return:
#   return(
#     subset(m, !rows_with_flagged_data, !cols_with_flagged_data)
#   )
# }


# TODO: also check in multi-omics that each matrix has same samples
# TODO: transform flagged data
# TODO: split negative matrix
