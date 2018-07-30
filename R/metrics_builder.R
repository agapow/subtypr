# Romain GUEDON
# metrics.R

# generate the metrics.RData
# so it builds the object metrics.list

#### utilities ####

entropy <- function(x) {
  # return entropy of vector, essentially number of bits required to specify
  if (length(x) == 0) {
    return(1.0)
  }

  freq_tbl <- table(x) / length(x)
  freq_vec <- as.vector(freq_tbl)
  return(-sum(freq_vec * log(freq_vec)))
}


mutual_information <- function(x, y) {
  # consistent with sklearn
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

homogeneity_completeness_vmeasure <- function(labels_true, labels_pred) {

  ## Preconditions & prep:
  stopifnot(length(labels_true) == length(labels_pred))
  cats_true <- factor(labels_true)
  cats_pred <- factor(labels_pred)

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

  return(list(homogeneity = homogeneity, completeness = completeness, vmeasure = vmeasure))
}





#### Create metrics.list ####


metrics.list <- list()



#### Average Silhouette Width ####


# for affinity matrix:

asw.affinity <- list(
  name = "asw.affinity", label = "Average silhouette width (affinity matrix)",
  maximize = TRUE, internal = TRUE
)


asw.affinity$Metric <- function(res, ground.truth = NULL, plot = F) {
  if (!plot) {
    return(summary(CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned))$avg.width)
  } else {
    silx <- CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned)
    plot(silx)
    return(summary(silx)$avg.width)
  }
}

metrics.list$asw.affinity <- asw.affinity

# for distance matrix:

asw.distance <- list(
  name = "asw.distance",
  label = "Average silhouette width (distance matrix)",
  maximize = TRUE,
  internal = TRUE
)

asw.distance$Metric <- function(res, ground.truth = NULL, plot = F) {
  cluster::silhouette(x = res$partition, dist = NULL, dmatrix = res$data.returned)
}

metrics.list$asw.distance <- asw.distance

#### Adjusted Rand Index ####

ari <- list(name = "ari", label = "Adjusted rand index", maximize = TRUE, internal = FALSE)

ari$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$corrected.rand
}


metrics.list$ari <- ari


#### Normalized Mutual Information ####

nmi <- list(name = "nmi", label = "Normalized mutual information", maximize = TRUE, internal = FALSE)
nmi$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  SNFtool::calNMI(x = res$partition, y = ground.truth)
}


metrics.list$nmi <- nmi

#### Meila's variation index VI ####

meilaVI <- list(name = "meilaVI", label = "Meila's variation index VI", maximize = TRUE, internal = FALSE)

meilaVI$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$vi
}


metrics.list$meilaVI <- meilaVI


#### Mutual Information ####

mi <- list(name = "mi", label = "Mutual Information", maximize = TRUE, internal = FALSE)


mi$Metric <- function(res, ground.truth, plot = F) {
  mutual_information(res$partition, ground.truth)
}

metrics.list$mi <- mi

#### Homogeneity ####

homogeneity <- list(name = "homogeneity", label = "Homogeneity", maximize = TRUE, internal = FALSE)

homogeneity$Metric <- function(res, ground.truth, plot = F) {

  # sub-functions
  mutual_information <- function(x, y) {
    # consistent with sklearn
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

  entropy <- function(x) {
    # return entropy of vector, essentially number of bits required to specify
    if (length(x) == 0) {
      return(1.0)
    }

    freq_tbl <- table(x) / length(x)
    freq_vec <- as.vector(freq_tbl)
    return(-sum(freq_vec * log(freq_vec)))
  }

  labels_pred <- res$partition
  labels_true <- ground.truth

  ## Preconditions & prep:
  stopifnot(length(labels_true) == length(labels_pred))
  cats_true <- factor(labels_true)
  cats_pred <- factor(labels_pred)


  ## Main:
  if (length(cats_true) == 0) {
    return(list(homogeneity = 1.0, completeness = 1.0, vmeasure = 1.0))
  }

  entropy_c <- entropy(cats_true)
  entropy_k <- entropy(cats_pred)

  mutual_info <- mutual_information(cats_true, cats_pred)

  homogeneity <- mutual_info / ifelse(0.0 < entropy_c, entropy_c, 1.0)
  return(homogeneity)
}

metrics.list$homogeneity <- homogeneity

#### Completeness ####

completeness <- list(name = "completeness", label = "Completeness", maximize = TRUE, internal = FALSE)
completeness$Metric <- function(res, ground.truth, plot = F) {

  # sub-functions
  mutual_information <- function(x, y) {
    # consistent with sklearn
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

  entropy <- function(x) {
    # return entropy of vector, essentially number of bits required to specify
    if (length(x) == 0) {
      return(1.0)
    }

    freq_tbl <- table(x) / length(x)
    freq_vec <- as.vector(freq_tbl)
    return(-sum(freq_vec * log(freq_vec)))
  }

  labels_pred <- res$partition
  labels_true <- ground.truth

  ## Preconditions & prep:
  stopifnot(length(labels_true) == length(labels_pred))
  cats_true <- factor(labels_true)
  cats_pred <- factor(labels_pred)


  ## Main:
  if (length(cats_true) == 0) {
    return(list(homogeneity = 1.0, completeness = 1.0, vmeasure = 1.0))
  }

  entropy_c <- entropy(cats_true)
  entropy_k <- entropy(cats_pred)

  mutual_info <- mutual_information(cats_true, cats_pred)

  completeness <- mutual_info / ifelse(0.0 < entropy_k, entropy_k, 1.0)
  return(completeness)
}

metrics.list$completeness <- completeness
#### V_measure ####

v_measure <- list(name = "v_measure", label = "V_measure", maximize = TRUE, internal = FALSE)
v_measure$Metric <- function(res, ground.truth, plot = F) {

  # sub-functions

  mutual_information <- function(x, y) {
    # consistent with sklearn
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

  entropy <- function(x) {
    # return entropy of vector, essentially number of bits required to specify
    if (length(x) == 0) {
      return(1.0)
    }

    freq_tbl <- table(x) / length(x)
    freq_vec <- as.vector(freq_tbl)
    return(-sum(freq_vec * log(freq_vec)))
  }

  labels_pred <- res$partition
  labels_true <- ground.truth

  ## Preconditions & prep:
  stopifnot(length(labels_true) == length(labels_pred))
  cats_true <- factor(labels_true)
  cats_pred <- factor(labels_pred)


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
  return(vmeasure)
}

metrics.list$v_measure <- v_measure

#### Dunn Index ####
dunn <- list(name = "dunn", label = "Dunn index", maximize = TRUE, internal = TRUE)

dunn$Metric <- function(res, ground.truth = NULL, plot = F) {
  fpc::cluster.stats(
    d = res$data.returned,
    clustering = res$partition,
    alt.clustering = ground.truth
  )$dunn
}


metrics.list$dunn <- dunn

#### Export metrics.list ####

save(... = metrics.list, file = "./inst/metrics/metrics.RData")
