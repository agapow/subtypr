#' Glioblastoma multiforme (GBM) multi-omics data
#'
#' The dataset comes from The Cancer Genome Atlas:
#'   \url{https://cancergenome.nih.gov/}.
#'
#' The dataset is curated level-three, see:
#'   \url{https://cancergenome.nih.gov/abouttcga/aboutdata/datalevelstypes}.
#'
#'
#' @format The dataset contains two lists:
#'
#' * $data_list: a list of 3 features matrix with patients in rows
#'   and features in columns:
#'
#'     * $mRNA: mRNA expression
#'     * $methylation: methylation profile
#'     * $miRNA: micro RNA expression
#'
#' * $survival: the survival data of the patients that contains:
#'
#'     * $PatientID: ID of TCGA patients
#'     * $Survival: survival time of the patients
#'     * $Death: death status (0=alive, 1=dead)
#'
#'
"gbm_data"
