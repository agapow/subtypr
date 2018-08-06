# Various metrics for assessing clustering compatiability



#' Return entropy of vector
#'
#' Essentially number of bits required to specify
#' Supporting function for metrics below
#'
#' @param x an integer vector
#'
#' @return The entropy of x
#'
#'
entropy <- function(x) {
  ## Preconditions & preparation:
  if (length(x) == 0) {
    return(1.0)
  }

  ## Main:
  freq_tbl <- table(x) / length(x)
  freq_vec <- as.vector(freq_tbl)

  return(-sum(freq_vec * log(freq_vec)))
}


#' Calculate mutual information of two vectors
#'
#' consistent with sklearn
#'
#' @param pred_partition partition predicted by a method
#' @param true_partition "true" partition, aka ground truth
#'
#' @return the mutual information of the two partitions
#' @export
#'
#'
mutual_information <- function(pred_partition, true_partition) {
  x <- pred_partition
  y <- true_partition

  vec_len <- length(x)
  df <- data.frame(x = x, y = y)
  cont_tbl <- table(df) / vec_len
  freqs_x <- rowSums(cont_tbl)
  freqs_y <- colSums(cont_tbl)

  sum_xy <- 0
  for (i in 1:nrow(cont_tbl)) {
    for (j in 1:ncol(cont_tbl)) {
      p_xy <- cont_tbl[i, j]
      if (0.0 < p_xy) {
        p_x <- freqs_x[i]
        p_y <- freqs_y[j]
        add_xy <- p_xy * log(p_xy / (p_x * p_y))
        sum_xy <- sum_xy + add_xy
      }
    }
  }

  return(sum_xy)
}

#' Normalized mutual information
#'
#' @param pred_partition partition predicted by a method
#' @param true_partition "true" partition, aka ground truth
#'
#' @return The normalized mutual information of the two partitions
#' @export
#'
normalized_mutual_information <- function(pred_partition, true_partition) {
  nmi <- mutual_information(pred_partition, true_partition) /
         sqrt(entropy(pred_partition) * entropy(true_partition))
  return(max(0, nmi, na.rm = TRUE))
}

#' Homogeneity, completeness and V-Measure metrics for partition membership
#'
#' Like scikitlearn, we'll assume the inputs are two vectors of categoricals,
#' or things that can be turned into categoricals.
#'
#' @param pred_partition partition predicted by a method
#' @param true_partition "true" partition, aka ground truth
#'
#' @return The homogeneity, the completeness and the v_measure
#'   of the two partitions.
#' @export
#'
#' @examples
#'   x <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4)
#'   y <- c(1, 1, 2, 2, 2, 2, 6, 7, 9, 9)
#'   homogeneity_completeness_vmeasure(x, y)
#'
homogeneity_completeness_vmeasure <- function(pred_partition, true_partition) {

  ## Preconditions & prep:
  stopifnot(length(true_partition) == length(pred_partition))
  cats_true <- factor(true_partition)
  cats_pred <- factor(pred_partition)

  ## Main:
  if (length(cats_true) == 0) {
    return(list(homogeneity = 1.0, completeness = 1.0, vmeasure = 1.0))
  }

  entropy_c <- entropy(cats_true)
  entropy_k <- entropy(cats_pred)

  mutual_info <- mutual_information(cats_true, cats_pred)

  homogeneity <- mutual_info / ifelse(0.0 < entropy_c, entropy_c, 1.0)
  completeness <- mutual_info / ifelse(0.0 < entropy_k, entropy_k, 1.0)


  if ((homogeneity + completeness) == 0.0) {
    vmeasure <- 0.0
  } else {
    vmeasure <- 2.0 * homogeneity * completeness / (homogeneity + completeness)
  }

  return(list(
    homogeneity = homogeneity,
    completeness = completeness,
    vmeasure = vmeasure
  ))
}


#' Classic silhouette index for distance matrix
#'
#' @param pred_partition partition predicted by a method
#' @param distance_matrix a distance matrix
#'
#' @return a silhouette object
#' @seealso \code{\link[cluster]{silhouette}}
#' @export
#'
silhouette_distance <- function(pred_partition, distance_matrix) {
  cluster::silhouette(
    x = pred_partition,
    dmatrix = distance_matrix
  )
}


#' Silhouette index for affinity matrix
#'
#' @param pred_partition partition predicted by a method
#' @param affinity_matrix an affinity matrix
#'
#' @return a silhouette object
#' @export
#'
silhouette_affinity <- function(pred_partition, affinity_matrix) {

  ## Preconditions & prepatation
  affinity_matrix <- as.matrix(affinity_matrix)
  affinity_matrix <- (affinity_matrix + t(affinity_matrix)) / 2
  diag(affinity_matrix) <- 0
  normalize <- function(X) X / rowSums(X)
  affinity_matrix <- normalize(affinity_matrix)
  n <- length(pred_partition)
  if (!all(pred_partition == round(pred_partition))) {
    stop("'pred_partition' must only have integer codes")
  }
  cluster_id <- sort(unique(pred_partition <- as.integer(pred_partition)))
  k <- length(cluster_id)
  if (k <= 1 || k >= n) {
    return(NA)
  }
  doRecode <- (any(cluster_id < 1) || any(cluster_id > k))
  if (doRecode) {
    pred_partition <- as.integer(fpred_partition <- factor(pred_partition))
  }
  cluster_id <- sort(unique(pred_partition))
  wds <- matrix(NA, n, 3, dimnames = list(names(pred_partition), c(
    "cluster",
    "neighbor",
    "sil_width"
  )))
  ## Main
  for (j in 1:k) {
    index <- (pred_partition == cluster_id[j])
    Nj <- sum(index)
    wds[index, "cluster"] <- cluster_id[j]
    dindex <- rbind(apply(affinity_matrix[!index, index,
      drop = FALSE
    ], 2, function(r) tapply(
        r, pred_partition[!index],
        mean
      )))
    maxC <- apply(dindex, 2, which.max)
    wds[index, "neighbor"] <- cluster_id[-j][maxC]
    s.i <- if (Nj > 1) {
      a.i <- colSums(affinity_matrix[index, index]) / (Nj - 1)
      b.i <- dindex[cbind(maxC, seq(along = maxC))]
      ifelse(a.i != b.i, (a.i - b.i) / pmax(b.i, a.i), 0)
    }
    else {
      0
    }
    wds[index, "sil_width"] <- s.i
  }

  ## Postconditions & return
  attr(wds, "Ordered") <- FALSE
  class(wds) <- "silhouette"
  wds
}

#' Adjusted rand index
#'
#' @param pred_partition partition predicted by a method
#' @param true_partition "true" partition, aka ground truth
#'
#' @return the adjusted rand index of the partitions
#' @export
#'
adjusted_rand_index <- function(pred_partition, true_partition) {
  if (is.null(true_partition)) stop("true_partition is missing")
  true_partition <- as.integer(true_partition)
  fpc::cluster.stats(
    d = NULL, clustering = pred_partition,
    alt.clustering = true_partition,
    compareonly = T
  )$corrected.rand
}

#' Meila's variation of information index
#'
#' @param pred_partition partition predicted by a method
#' @param true_partition "true" partition, aka ground truth
#'
#' @return the Meila's variation of information index of the partitions
#' @export
#'
meila_vi <- function(pred_partition, true_partition) {
  if (is.null(true_partition)) stop("true_partition is missing")
  true_partition <- as.integer(true_partition)
  fpc::cluster.stats(
    d = NULL,
    clustering = pred_partition,
    alt.clustering = true_partition,
    compareonly = T
  )$vi
}

#' Dunn index
#'
#' @param pred_partition partition predicted by a method
#' @param distance_matrix a distance matrix
#'
#' @return the Dunn index of the partitions
#' @export
#'
dunn_index <- function(pred_partition, distance_matrix) {
  fpc::cluster.stats(
    d = distance_matrix,
    clustering = pred_partition
  )$dunn
}
