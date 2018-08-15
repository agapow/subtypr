# Methods wrapped to be used in a uniformized way



#' Subtypes patients using Similarity Network Fusion
#'
#' @param data_list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#'
#' @param cluster_number The supposed or previously infered number of clusters.
#'
#' @param K Number of neighbors in K-nearest neighbors part of the algorithm of
#'   fusion and for the computation of the affinity matrix (same parameter for
#'   both process).
#'
#' @param alpha Variance for the local model (for the Gaussian kernel of the
#'   affinity matrix). Recommended values are between 0.3 and 0.8.
#'
#' @param t Number of iterations for the diffusion process.
#'
#' @param spectral_clust_type The type of spectral clustering, see
#'   \code{\link[SNFtool]{spectralClustering}} for more information.
#'
#' @param minimal_return logical, if TRUE, the result of the function will just
#'   be what's needed to evaluate the goodness of the partition, i.e. the
#'   partition and the element for internal metrics.
#'
#' @return a result list containing:
#'   * $partition: The predicted partition
#'   * $element_for_metric: The name of the element in the result list.
#'     containing the data to be used with internal metrics.
#'   * $affinity_fused: The fused affinity matrix returned by the function
#'     \code{\link[SNFtool]{SNF}}.
#'
#' @seealso \code{\link[SNFtool]{SNF}},
#'   \code{\link[SNFtool]{spectralClustering}},
#'   \code{\link[SNFtool]{affinityMatrix}}.
#'
#' @export

subtype_snf <- function(data_list,
                        minimal_return = FALSE,
                        cluster_number,
                        K = 20,
                        alpha = 0.5,
                        t = 20,
                        spectral_clust_type = 3) {


  ## Main:

  # SNF process
  affinity_fused <- SNFtool::SNF(
    Wall = lapply(
      lapply(data_list, function(data) SNFtool::dist2(
          as.matrix(data),
          as.matrix(data)
        )),
      function(dist) SNFtool::affinityMatrix(
          diff = dist,
          K = K,
          sigma = alpha
        )
    ),
    K = K,
    t = t
  )
  # Spectral clustering
  partition <- SNFtool::spectralClustering(
    affinity = affinity_fused,
    K = cluster_number,
    type = spectral_clust_type
  )

  return(list(
    partition = partition,
    element_for_metric = "affinity_fused",
    affinity_fused = affinity_fused
  ))
}


#' Subtypes using Affinity Network Fusion
#'
#' @param data_list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#'
#' @param cluster_number The supposed or previously infered number of clusters.
#'
#' @param k_affi The number of k-nearest neighbors for the affinity matrix
#'   calculation.
#'
#' @param alpha_affi Coefficient for local diameters for affinity matrix
#'   calculation. Default value: 1/6.
#'
#'
#' @param beta_affi  Coefficient for pair-wise distance for affinity matrix
#'   calculation. Default value: 1/6.
#'
#'
#' @param k_fusion the number of k nearest neighbors for function kNN_graph
#'
#' @param weigth_fusion a list of non-negative real numbers (which will be
#'   normalized internally so that it sums to 1) that one-to-one correspond to
#'   the affinity matrices computed from the features matrices provided in
#'   `data_list`. If not set, internally uniform weights are assigned to all
#'   the affinity matrices.
#'
#' @param type_fusion choose one of the two options "one-step" random
#'   walk, or "two-step" random walk (for the fusion process).
#'
#' @param alpha_fusion a list of eight non-negative real numbers (which will be
#'   normalized internally to make it sums to 1). Only used when "two-step"
#'   (default value of `type_fusion`) random walk is used. `alpha_fusion` is the
#'   weights for eight terms in the "two-step" random walk formula (check
#'   research paper for more explanations about the terms).
#'   Default value: (1, 1, 0, 0, 0, 0, 0, 0), i.e., only use the first two terms
#'    (since they are most effective in practice).
#'
#' @param spectral_type choose one of three versions of graph Laplacian:
#'   "unnormalized": unnormalized graph Laplacian matrix (L = D - W`);
#'   "rw": normalization closely related to random walk (L = I - D^(-1)*W)`;
#'   (default choice) "sym": normalized symmetric matrix
#'   (L = I - D^(-0.5) * W * D^(-0.5)`)
#'   For more information:
#'   \url{https://www.cs.cmu.edu/~aarti/Class/10701/readings/Luxburg06_TR.pdf}
#'
#' @param verbose_fusion logical(1); if true, print some information concerning
#'   the fusion step.
#'
#' @param minimal_return logical, if TRUE, the result of the function will just
#'   be what's needed to evaluate the goodness of the partition, i.e. the
#'   partition and the element for internal metrics.
#'
#'
#' @return a result list containing:
#'   * $partition: The predicted partition
#'   * $element_for_metric: The name of the element in the result list.
#'     containing the data to be used with internal metrics.
#'   * $affinity_fused: The fused affinity matrix returned by the function
#'     \code{\link[ANF]{ANF}}
#'
#' @seealso \code{\link[ANF]{ANF}}, \code{\link[ANF]{affinity_matrix}},
#'   \code{\link[ANF]{spectral_clustering}}.
#'
#' @export
#'
subtype_anf <- function(data_list,
                        minimal_return = FALSE,
                        cluster_number,
                        k_affi,
                        alpha_affi = 1 / 6,
                        beta_affi = 1 / 6,
                        k_fusion = 20,
                        weigth_fusion = NULL,
                        type_fusion = c("two-step", "one-step"),
                        alpha_fusion = c(1, 1, 0, 0, 0, 0, 0, 0),
                        spectral_type = c("rw", "sym", "unnormalized"),
                        verbose_fusion = FALSE) {
  type_fusion <- match.arg(type_fusion)
  spectral_type <- match.arg(spectral_type)

  affinity_list <- lapply(data_list, function(data) ANF::affinity_matrix(
      D = as.matrix(dist(data)),
      k = k_affi,
      alpha = alpha_affi,
      beta = beta_affi
    ))

  affinity_fused <- ANF::ANF(
    Wall = affinity_list, K = k_fusion, weight = weigth_fusion,
    type = type_fusion, alpha = alpha_fusion, verbose = verbose_fusion
  )

  partition <- ANF::spectral_clustering(
    A = affinity_fused,
    k = cluster_number,
    type = spectral_type
  )

  return(list(
    partition = partition,
    element_for_metric = "affinity_fused",
    affinity_fused = affinity_fused
  ))
}


# To set up the parameters of PerturbationClustering:
#   * `ncore` Number of cores that the algorithm should use.
# Default value is 2.
#
# * `clusteringMethod`	The name of built-in clustering algorithm that
# PerturbationClustering will use. Currently supported algorithm are kmeans,
# pam and hclust. Default value is "kmeans".
#
# * `clusteringFunction`	The clustering algorithm function that will be used
# instead of built-in algorithms.
# * `clusteringOptions` A list of parameter will be passed to the clustering
# algorithm in clusteringMethod.
# * `perturbMethod` The name of built-in perturbation method that
# PerturbationClustering will use, currently supported methods are noise and
# subsampling. Default value is "noise".
# * `perturbFunction` The perturbation method function that will be used
# instead of built-in ones.
# * `perturbOptions` A list of parameter will be passed to the perturbation
# method in perturbMethod.
# * `iterMin` The minimum number of iterations. Default value is 20.
# * `iterMax` The maximum number of iterations. Default value is 200.
# * `madMin` The minimum of Mean Absolute Deviation of AUC of Connectivity
# matrix for each k. Default value is 1e-03.





#' Subtypes using PINSPlus package
#'
#' Perform subtyping using multiple types of data
#'
#' subtype_pins uses \code{\link[PINSPlus]{SubtypingOmicsData}}. The input is a
#'   list of data matrices where each matrix represents the molecular
#'   measurements of a data type. The input matrices must have the same number
#'   of rows. The function aims to find the optimum number of subtypes
#'   and location of each sample in the clusters from integrated input data
#'   dataList through two processing stages:
#'
#'   * Stage I: The algorithm first partitions each data type using the function
#'     PerturbationClustering. It then merges the connectivities across data
#'     types into similarity matrices. Both kmeans and similarity-based
#'     clustering algorithms - partitioning around medoids pam are used to
#'     partition the built similarity. The algorithm returns the partitioning
#'     that agrees the most with individual data types.
#'
#'   * Stage II: The algorithm attempts to split each discovered group if there
#'     is a strong agreement between data types, or if the subtyping in Stage I
#'     is very unbalanced.
#'
#'
#'
#' @param data_list a list of data matrices with continuous data of format
#'   samples x features (with the same number of samples).
#'
#' @param minimal_return logical, if TRUE, the result of the function will just
#'   be what's needed to evaluate the goodness of the partition, i.e. the
#'   partition and the element for internal metrics.
#'
#' @param return_stage_2 logical to return the partition of the stage 2 of the
#'   PINS method's workflow.
#' @param k_max The maximum number of clusters tested (from 2 to k_max).
#'   Default value is 5.
#' @param agreement_cutoff Agreement threshold to be considered consistent.
#'   Default value is 0.5.
#' @param verbose Set it to TRUE of FALSE to get more or less details
#'   respectively.
#' @param ... these arguments will be passed to PerturbationClustering
#'   algorithm. See \code{\link[PINSPlus]{PerturbationClustering}}.
#'
#'
#'
#'
#'
#' @return a result list containing:
#'   * $partition: The predicted partition
#'   * $element_for_metric: The name of the element in the result list.
#'     containing the data to be used with internal metrics.
#'   * $dataTypeResult: A list of results for individual data type.
#'     Each element of the list is the result of
#'     \code{\link[PINSPlus]{PerturbationClustering}}
#'     for the corresponding data matrix provided in dataList.
#'
#' @export
#'
#' @seealso \code{\link[PINSPlus]{PerturbationClustering}},
#'   \code{\link[PINSPlus]{SubtypingOmicsData}}.
#'
subtype_pins <- function(data_list,
                         minimal_return = FALSE,
                         return_stage_2 = TRUE,
                         k_max = 5,
                         agreement_cutoff = 0.5,
                         verbose = T, ...) {
  result <- PINSPlus::SubtypingOmicsData(
    dataList = data_list,
    kMax = k_max,
    agreementCutoff = agreement_cutoff,
    verbose = verbose,
    ... = ...
  )
  if (minimal_return) {
    list(
      partition = if (return_stage_2) result$cluster2 else result$cluster1,
      element_for_metric = NULL
    )
  } else {
    list(
      partition = if (return_stage_2) result$cluster2 else result$cluster1,
      element_for_metric = NULL,
      data_type_result = result$dataTypeResult
    )
  }
}








