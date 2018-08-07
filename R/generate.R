# Generation of synthetic datasets for testing methods


#' Generate a partitions matrix
#'
#' The intersection of each partition (one per line) is a partition
#'
#' It's used to create complementary partitions inside data type. The idea is
#'   that this complementary partitions intersect into a global partition that
#'   should
#'   appear when a multi omic data integration method is used.
#'
#' @param n_cluster The number of cluster.
#' @param n_samples The number of samples
#' @param n_layers The number of layers
#'
#' @return a list:
#'   * $partition_tot: the intersection of the partitions
#'   * $partitions: a matrix with rows representing the partition for each
#'     layer.
#'
generate_partitions <- function(n_samples, n_layers, n_cluster = 4) {

  if (n_cluster != 4) stop("only built for n_cluster == 4")

  # n_samples == group_size * n_cluster + rest_size
  group_size <- n_samples %/% n_cluster
  rest_size <- n_samples %% n_cluster


  # Either type 1: 11112222 or type 2: 11221122, the first half of the data
  # layers are type 1 and the second half is type 2:
  layer_structure <- c(rep(1, n_layers %/% 2),
                       rep(2, n_layers - n_layers %/% 2))

  partitions <- matrix(0, n_layers, n_samples)
  for (i in 1:n_layers) {
    switch(layer_structure[i],
           # case 1
           partitions[i, ] <- c(
             rep(1, group_size * (n_cluster / 2)),
             rep(2, group_size * (n_cluster / 2)),
             rep(2, rest_size)
           ),
           # case 2
           partitions[i, ] <- c(
             rep(1, group_size),
             rep(2, group_size),
             rep(1, group_size),
             rep(2, group_size),
             rep(2, rest_size)
           )
    )
  }


  # colsum is the same if they belong to the same cluster:
  partition_tot <- colSums(partitions)

  # re-scale cluster indicator from 1 to n_cluster:
  partition_tot <- as.integer(as.factor(partition_tot))
  list(partition_tot = partition_tot,
       partitions = partitions)
}


#' Generate simulated data
#'
#' Generate simulated data as in 10.1021/acs.jproteome.5b00824 (DOI).
#'
#' Considering a partition of the samples, the data will be modified to have a
#'   structure corresponding to this partition.
#'
#' @param X A feature matrix, rows are patients, columns are features.
#'
#' @param partition A partition of the samples.
#'
#' @param sd_signal The standard deviation of the signal in each cluster.
#'
#' @param sparse logical, to create sparsity in singular vector of Xsim.
#'
#' @param percent_sparsity percentage of sparsity in singular vector of Xsim.
#'
#' @seealso DOI: 10.1021/acs.jproteome.5b00824
#'
#' @return a simulated structured matrix
#'
generate_single_moclust <- function(X, partition, sd_signal = 0.5,
                                    sparse = FALSE, percent_sparsity = 0.3) {

  ## Preconditions & preparation
  n_samples <- nrow(X)
  n_features <- ncol(X)

  n_cluster <- length(unique(partition))
  partition <- as.integer(as.factor(partition))

  # check


  ## Main
  svd_X <- svd(X)
  d_modified <- c(svd_X$d[1],
                  matrix(mean(svd_X$d[2:length(svd_X$d)]),
                         1,
                         length(svd_X$d) - 1))



  # x_tilde generation:
  x_tilde <- matrix(0, n_cluster, n_features)
  for (c in 1:n_cluster){
    x_tilde[c, ] <- rnorm(n_features, 0, sd_signal)
  }

  # X_sim generation:
  X_sim <- matrix(0, n_samples, n_features)
  for (i in 1:n_samples){
    X_sim[i, ] <- x_tilde[partition[i], ] + rnorm(n_features)
  }

  svd_X_sim <- svd(X_sim)
  # Sparsity ?
  if (sparse) {
    # no sparsity if less features than samples:
    if (n_features >= n_samples) {
      n_sparse <- round(n_features * percent_sparsity)
      sparse_index <- sample(x = n_features, size = n_sparse)
      svd_X$v[sparse_index, ] <- matrix(0, n_sparse, n_samples)
    }

  }

  # Combine:
  X_generated <- svd_X_sim$u %*% diag(d_modified) %*% t(svd_X$v)

  ## Postconditions & return

  X_generated
}



#' Generate simulated data
#'
#' Generate simulated data as in 10.1021/acs.jproteome.5b00824
#'
#' @param data_support a list of features matrices
#' @param sd_signal the standard deviation of the signal in each cluster,
#'   see 10.1021/acs.jproteome.5b00824, try 1, 0.5 or 0.2 for good to bad signal
#'    to noise ratio
#' @param sparse logical, to create sparsity in singular vector of Xsim,
#'   see 10.1021/acs.jproteome.5b00824
#'
#' @param percent_sparsity a vector (value for each data type) indicating the
#'   percentage of sparsity for the singular vector V of Xsim. See
#'   10.1021/acs.jproteome.5b00824
#'
#' @return a list of simulated structured data, the corresponding partition
#'   (partition_tot) and the partition per layer (partitions).
#'
generate_multi_moclust <- function(data_support, sd_signal = 0.5,
                                   sparse = FALSE,
                                   percent_sparsity) {

  ## Preconditions & preparation
  n_layers <- length(data_support)
  n_samples <- dim(data_support[[1]])[1] # samples in the rows
  if (isTRUE(sparse) && is.null(percent_sparsity)) {
    stop("please provide percent_sparsity!")
  }
  # Partition generation:
  partitions_list <- generate_partitions(n_samples, n_layers)
  partition_tot <- partitions_list$partition_tot
  partitions <- partitions_list$partitions

  ## Main
  data_generated <- vector('list', n_layers)
  for (l in 1:n_layers){
    data_generated[[l]] <-
      generate_single_moclust(X = data_support[[l]],
                              partition = partitions[l, ],
                              sd_signal = sd_signal,
                              sparse = sparse,
                              percent_sparsity = percent_sparsity[l])
  }

  ## Postconditions & return
  return(list(data_generated = data_generated,
       partition_tot = partition_tot,
       partitions = partitions))
}



#' Generate synthetic data based on a list of features matrics.
#'
#' Generate a structured list of data with 4 clusters.
#'
#' For the moment, only one method of generation is provided:
#'
#'  * "moclus" is inspired from
#'   moCluster: Identifying Joint Patterns Across Multiple Omics Data Sets, DOI:
#'   10.1021/acs.jproteome.5b00824. The single parameter is: sd_signal, see
#'   the article.
#'
#' @param data_support a list of features matrices used to generate the
#'   data
#' @param structure_type the type of structure,
#' @param ... parameters for the method of generation. See Details.
#' @return a list of synthetic features matrix with the same dimension than the
#'   `data_support`
#'
generate_multi_structured <- function(data_support,
                                       structure_type = c("moclus"),
                                       ...) {

  ## Preconditions & preparation:
  structure_type <- match.arg(structure_type)

  ## Main:

  if (structure_type == "moclus") {
    return(generate_multi_moclust(data_support = data_support,
                                  ...))
  }
}



#' Generate a list of synthetic data matrices for validation
#'
#' Generate a list of unstructured data matrices of the desired dimensions.
#'
#' Two types of distribution are available:
#'   * "gaussian": generates a matrix with each patients having features with
#'   the same gaussian distribution.
#'   * "uniform" generates a matrix with each patients having features with
#'   the same uniform distribution.
#'
#' @param type type of synthetic data generated, "gaussian" or "uniform".
#' @param n_layers The number of layers (features matrix) to
#'   be generated.
#' @param n_samples The number of samples.
#' @param n_features integer vector. The number of features per layer.
#'
#' @return a list of unstructured matrix of the type selected in `type`
#'   (gaussian by default).
#'
generate_multi_unstructured <- function(type = c("gaussian", "uniform"),
                                         n_samples, n_features, n_layers) {
  type <- match.arg(type)

  data_list <- vector("list", n_layers)
  for (t in 1:n_layers) {
    h <- n_features[t]
    # synthetization
    if (type == "gaussian") {
      data <- matrix(0, n_samples, h)
      for (j in 1:h) {
        data[, j] <- rnorm(n_samples)
      }
    } else if (type == "uniform") {
      data <- matrix(0, n_samples, h)
      for (j in 1:h) {
        data[, j] <- runif(n_samples)
      }
    }
    data_list[[t]] <- data
  }
  data_list
}


#' Generate 'multi-omic' synthetic data matrices  for validation
#'
#' For validation and testing multi-omic / integrative methods, we need
#'
#' @param n_samples  the number of samples (rows)
#'
#' @param n_features a list of the numbers of features for each matrix
#'
#' @param type type of data, "gaussian" is a homogeneous group of patients,
#'   made with the gaussian distribution. "structured" is made as the
#'   simulated data is created in 10.1021/acs.jproteome.5b00824
#'
#' @param support_data_list a list of features matrix that are used to generate
#'   the data
#'
#' @param n_layers integer. The number of layers (features matrix) to be
#'   generated
#'
#' @param separation double. The value of separation between clusters:
#'   the space between the means of the normal distributions used to
#'   generate the
#'   structure of the synthetic data.
#'
#' @return 'multi-omic' synthetic data matrices for validation
#'
#' @export
generate_synth_data <- function(type = c("gaussian", "uniform", "structured"),
                                n_samples = NULL,
                                n_features = NULL,
                                n_layers = NULL,
                                support_data_list = NULL,
                                separation = 2) {
  ## Preconditions & preparation:
  type <- match.arg(type)
  if (type %in% c("uniform", "gaussian")) {
    assert_that(
      !is.null(n_samples), 2 <= n_samples,
      !is.null(n_features), 1 <= n_features,
      !is.null(n_layers), 1 <= n_layers,
      msg = "size and number of output matrices required"
    )
  }
  if (type %in% c("structured")) {
    assert_that(
      !is.null(support_data_list),
      msg = "support_data_list missing"
    )
  }


  ## Main:
  if (type == "gaussian") {
    data_list <- generate_matrix_unstructured(
      type = "gaussian",
      n_samples, n_features, n_layers
    )

    res <- list(data_list = data_list, partition = NULL)
  } else if (type == "uniform") {
    data_list <- generate_matrix_unstructured(
      type = "uniform",
      n_samples, n_features, n_layers
    )
    res <- list(data_list = data_list, partition = NULL)
  } else if (type == "structured") {
    res <- generate_matrix_structured(
      data_support = support_data_list,
      structure_type = "basic",
      separation = separation
    )
  }

  ## Postconditions & return:
  res
}
