# Generation of synthetic datasets for testing methods

#' Generate synthetic data
#'
#' Generate synthetic data with a known structure based on a real dataset provided by the user
#'
#' @param data.support a list of features matrix that are used to generate the data
#' @param structure.type the type of structure, "moClus" is inspired from
#'   moCluster: Identifying Joint Patterns Across Multiple Omics Data Sets,
#'   DOI: 10.1021/acs.jproteome.5b00824
#' @param separation adjust the separation between clusters.
#'   It's the space between the means of the normal distributions used to generate the
#'   structure of the synthetic data
#'
#' @return a list of synthetic features matrix with the same dimension than the `data.support`
Generator_Support_based <- function(data.support,
                                structure.type = c("basic", "moClus"),
                                separation = 2) {

  structure.type <- match.arg(structure.type)

  n_layers <- length(data.support)
  n_samples <- dim(data.support[[1]])[1] # samples in the rows

  # TYPE 1
  if (structure.type == "basic") {
    k = 4 #fixed for the moment
    group.size <- n_samples %/% k
    n_samples_sim <- group.size*k #number of patients keeped for the synthetic data
    c <- n_layers %/% 2
    cpt <- 2 # class per type

    # partition
    layer.structure <- c(rep(1,c), rep(2, n_layers - c))
    partitions <- matrix(0, n_layers, n_samples_sim)
    for (i in 1:n_layers) {
      switch(layer.structure[i],
             # case 1
             partitions[i,] <- c(rep(1, group.size*(k/2)),
                                 rep(2, group.size*(k/2))),
             # case 2
             partitions[i,] <- c(rep(1, group.size),
                                 rep(2, group.size),
                                 rep(1, group.size),
                                 rep(2, group.size))
             )
    }

    partition.tot <- colSums(partitions)# colsum is the same if they belong to the same cluster
    partition.tot <- as.integer(as.factor(partition.tot)) # re-scale cluster indicator from 1 to k


    means <- matrix(0, n_layers, cpt)
    for (l in 1:n_layers) {
      means[l,] <- sample(seq(0, (k-1)*separation, separation), cpt)
    }



    # synthetization according to the partition and the means
    Xgens <- vector('list', n_layers)
    for (x in 1:n_layers) {

      X <- data.support[[x]][sample(n_samples_sim), ]# pick randomly the correct number of samples

      n_features <- dim(X)[2]

      # first svd
      svd.X <- svd(X)

      Xsim <- matrix(0, n_samples_sim, n_features)

      for (i in 1:n_samples_sim){
        Xsim[i,] <- matrix(means[x, partitions[x,i]], 1, n_features) + rnorm(n_features)
      }
      Xgens[[x]] <- svd(Xsim)$u %*% diag(svd.X$d) %*% t(svd.X$v)
    }

  # TYPE 2
  } else if (type == "moClus") {


  }

  return(list(data.list = Xgens, partition = partition.tot))
}



#' Generate synthetic data matrix for validation
#'
#' @param dims a vector of 2 integers, which are the dimensions of the matrix to
#'   be generated, being the number of samples and features respectively
#'
#' @param type type of synthetic data generated : "gaussian" generates a matrix
#'   with each patients having features with the same gaussian distribution
#'   and "uniform" generates a matrix with each patients having features with
#'   the same uniform distribution.
#'
#' @return a synthetic matrix of the type selected in `type`
#'   (gaussian by default) with dimensions `dims`
Generator_unstructured <- function(type = c("gaussian", "uniform"), dims) {

  type = match.arg(type)

  n <- dims[1]
  h <- dims[2]

  # synthetization
  if (type == "gaussian") {
    data <- matrix(0, n, h)
    for (j in 1:h) {
      data[,j] <- rnorm(n)
    }
  } else if (type == "uniform") {
    data <- matrix(0, n, h)
    for (j in 1:h) {
      data[,j] <- runif(n)
    }
  }
  data
}


#' Generate 'multi-omic' synthetic data matrices  for validation
#'
#' For validation and testing multi-omic / integrative methods, we need
#'
#' @param n_samples  the number of samples (rows)
#' @param n_features a list of the numbers of features for each matrix
#' @param type type of data, "gaussian" is a homogeneous group of patients, made with the gaussian distribution. "structured" is made as the
#'   simulated data is created in 10.1021/acs.jproteome.5b00824
#' @param support.data.list a list of features matrix that are used to generate the data
#' @param n_layers the number of layers (features matrix) to be generated
#' @param separation adjust the separation between clusters when `type` = "structured".
#'   It's the space between the means of the normal distributions used to generate the
#'   structure of the synthetic data
#'
#' @return 'multi-omic' synthetic data matrices for validation
#' @export
GenerateSynthData <- function(type = c("gaussian", "uniform", "structured"),
                                      support.data.list = NULL,
                                      n_samples = NULL, n_layers = NULL,
                                      n_features = NULL,
                                      separation = 2) {
  type = match.arg(type)

  if (type == "gaussian") {
    if (any(is.null(n_samples), is.null(n_layers))) {
      stop("n_sample, n_layers and n_features are all required !")
    }
    data.list <- lapply(n_features, function(h) matrix(0, n_samples, h))
    for (l in 1:n_layers) {
      data.list[[i]] <- generate_synth_data(type = "gaussian",
                                            dims = dim(datalist[[i]]))
    }
    res <- list(data.list = data.list, permutation = NULL)

    } else if (type == "uniform") {
      if (any(is.null(n_samples), is.null(n_layers))) {
        stop("n_sample, n_layers and n_features are all required !")
      }
      data.list <- lapply(n_features, function(h) matrix(0, n_samples, h))
      for (l in 1:n_layers) {
        data.list[[i]] <- generate_synth_data(type = "uniform",
                                              dims = dim(datalist[[i]]))
      }
      res <- list(data.list = data.list, permutation = NULL)

    } else if (type == "structured") {
      if (is.null(support.data.list)) stop("support.data.list is missing !")
      res <- synth_based_on_data(data.support = support.data.list,
                                 structure.type = "basic", separation = separation)

    }
  res
}







