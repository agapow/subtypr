# Generation of synthetic datasets for testing methods

#' Generate synthetic data based on a real dataset.
#'
#' @param data.support a list of features matrices used to generate the
#'   data
#' @param structure.type the type of structure, "moClus" is inspired from
#'   moCluster: Identifying Joint Patterns Across Multiple Omics Data Sets, DOI:
#'   10.1021/acs.jproteome.5b00824
#' @param separation double. The value of separation between clusters: the space
#'   between the means of the normal distributions used to generate the
#'   structure of the synthetic data.
#'
#' @return a list of synthetic features matrix with the same dimension than the
#'   `data.support`
#'
generate_matrix_structured <- function (data.support,
                                    structure.type = c("basic", "moClus"),
                                    separation = 2) {
  ## Preconditions & preparation:
  structure.type <- match.arg (structure.type)

  n_layers <- length (data.support)
  n_samples <- dim (data.support[[1]])[1] # samples in the rows

  ## Main:
  # TYPE 1
  if (structure.type == "basic") {
    k <- 4 # fixed for the moment
    group.size <- n_samples %/% k
    n_samples_sim <- group.size * k # number of patients keeped for the synthetic data
    c <- n_layers %/% 2
    cpt <- 2 # class per type

    # partition
    layer.structure <- c(rep(1, c), rep(2, n_layers - c))
    partitions <- matrix(0, n_layers, n_samples_sim)
    for (i in 1:n_layers) {
      switch(layer.structure[i],
        # case 1
        partitions[i, ] <- c(
          rep(1, group.size * (k / 2)),
          rep(2, group.size * (k / 2))
        ),
        # case 2
        partitions[i, ] <- c(
          rep(1, group.size),
          rep(2, group.size),
          rep(1, group.size),
          rep(2, group.size)
        )
      )
    }

    partition.tot <- colSums(partitions) # colsum is the same if they belong to the same cluster
    partition.tot <- as.integer(as.factor(partition.tot)) # re-scale cluster indicator from 1 to k


    means <- matrix(0, n_layers, cpt)
    for (l in 1:n_layers) {
      means[l, ] <- sample(seq(0, (k - 1) * separation, separation), cpt)
    }



    # synthetization according to the partition and the means
    Xgens <- vector("list", n_layers)
    for (x in 1:n_layers) {
      X <- data.support[[x]][sample(n_samples_sim), ] # pick randomly the correct number of samples

      n_features <- dim(X)[2]

      # first svd
      svd.X <- svd(X)

      Xsim <- matrix(0, n_samples_sim, n_features)

      for (i in 1:n_samples_sim) {
        Xsim[i, ] <- matrix(means[x, partitions[x, i]], 1, n_features) + rnorm(n_features)
      }
      Xgens[[x]] <- svd(Xsim)$u %*% diag(svd.X$d) %*% t(svd.X$v)
    }

    # TYPE 2
  } else if (type == "moClus") {
    stop("not implemented yet, sorry")
  }

  ## Postconditions & return:
  return (list (data.list = Xgens, partition = partition.tot))
}



#' Generate a list of synthetic data matrices for validation
#'
#' @param dims a vector of 2 integers, which are the dimensions of the matrix to
#'   be generated, being the number of samples and features respectively
#'
#' @param type type of synthetic data generated : "gaussian" generates a matrix
#'   with each patients having features with the same gaussian distribution
#'   and "uniform" generates a matrix with each patients having features with
#'   the same uniform distribution.
#' @param n_layers integer. The number of layers (features matrix) to be generated.
#'
#' @return a list of `n_layers` synthetic matrix of the type selected in `type`
#'   (gaussian by default) with dimensions `dims`.
#'
generate_matrix_unstructured <- function (type = c("gaussian", "uniform"), n_samples, n_features, n_layers) {
  type <- match.arg (type)

  data.list <- vector("list", n_layers)
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
    data.list[[t]] <- data
  }
  data.list
}


#' Generate 'multi-omic' synthetic data matrices  for validation
#'
#' For validation and testing multi-omic / integrative methods, we need
#'
#' @param n_samples  the number of samples (rows)
#' @param n_features a list of the numbers of features for each matrix
#' @param type type of data, "gaussian" is a homogeneous group of patients, made
#'   with the gaussian distribution. "structured" is made as the simulated data
#'   is created in 10.1021/acs.jproteome.5b00824
#' @param support.data.list a list of features matrix that are used to generate
#'   the data
#' @param n_layers integer. The number of layers (features matrix) to be
#'   generated
#' @param separation double. The value of separation between clusters: the space
#'   between the means of the normal distributions used to generate the
#'   structure of the synthetic data.
#'
#' @return 'multi-omic' synthetic data matrices for validation
#' @export
generate_synth_data <- function(type = c("gaussian", "uniform", "structured"),
                              n_samples = NULL,
                              n_features = NULL,
                              n_layers = NULL,
                              support.data.list = NULL,
                              separation = 2) {
  ## Preconditions & preparation:
  type <- match.arg (type)
  if (type %in% c('uniform', 'gaussian')) {
    assert_that (
      ! is.null (n_samples), 2 <= n_samples,
      ! is.null (n_features), 1 <= n_features,
      ! is.null (n_layers), 1 <= n_layers,
      msg='size and number of output matrices required'
    )
  }
  if (type %in% c('structured')) {
    assert_that (
      ! is.null (support.data.list),
      msg='support.data.list missing'
    )
  }


  ## Main:
  if (type == "gaussian") {

      data.list <- generate_matrix_unstructured (type = "gaussian", n_samples, n_features, n_layers)
      res <- list (data.list = data.list, partition = NULL)

  } else if (type == "uniform") {

    data.list <- generate_matrix_unstructured (type = "uniform", n_samples, n_features, n_layers)
    res <- list (data.list = data.list, partition = NULL)

  } else if (type == "structured") {

    res <- generate_matrix_structured (
      data.support = support.data.list,
      structure.type = "basic",
      separation = separation
    )
  }

  ## Postconditions & return:
  res
}
