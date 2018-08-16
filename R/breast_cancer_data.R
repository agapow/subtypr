#' Breast cancer multi-omics data
#'
#' The dataset comes from The Cancer Genome Atlas:
#'   \url{https://cancergenome.nih.gov/}.
#'
#'
#' @format The data contains a training and a testing set, resulting in
#'   4 elements:
#'
#' * $data_list that contains:
#'
#'   * $mRNA: mRNA expression
#'   * $methylation: methylation profile
#'   * $miRNA: micro RNA expression
#'
#' * $partition: the classification of patients according to
#'   \url{https://doi.org/10.1073/pnas.191367098}
#'
#' * $test_data_list that contains:
#'
#'   * $mRNA: mRNA expression
#'   * $methylation: methylation profile
#'   * $miRNA: micro RNA expression
#'
#' * $test_partition: the classification of patients according to
#'   \url{https://doi.org/10.1073/pnas.191367098}
#'
#'
"breast_cancer_data"
