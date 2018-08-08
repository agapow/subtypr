#' Plot the distribution of the features
#'
#' The function plots the distribution of the mean, the variance and the median
#'  absolute deviation of the features.
#'
#'
#' @param data a feature matrix with samples in rows and features in columns
#' @param thickness numeric, to adjust the size of the bins:
#'   #bins = #features * thickness
#' @param split_figures logical, to have all the plot on three different plot.
#'
#' @export
#'
check_distribution <- function(data, thickness = 0.1, split_figures = FALSE) {
  mean_features <- apply(data, 2, function(x) mean(x, na.rm = TRUE))
  variance_features <- apply(data, 2, function(x) var(x, na.rm = TRUE))
  MAD_features <- apply(data, 2, function(x) mad(x, na.rm = TRUE))
  n_features <- ncol(data)
  if (!split_figures) par(mfrow = c(3, 1))
  hist(mean_features,
    breaks = n_features * thickness,
    col = "red",
    main = "data (mean) distribution", xlab = "Average value of features"
  )
  hist(variance_features,
    breaks = n_features * thickness,
    col = "red",
    main = "data (Variance) distribution", xlab = "Variance of features"
  )
  hist(MAD_features,
    breaks = n_features * thickness,
    col = "red",
    main = "data (MAD) distribution",
    xlab = "Median absolute deviation of features"
  )
  par(mfrow = c(1, 1))
}




#' Box-plot of clinical features for a given partition of the samples.
#'
#' Plot the box-plot of the given clinical features, splitting data according
#'  to the given partition.
#'
#' @param clinical_feature a numeric vector with a value for each patients.
#' @param partition an integer (or factor) vector indicating the group of
#'  each patient.
#' @param dot logical, to plot the points on the box-plot
#' @param feature_name the name of the clinical feature plotted.
#' @param group_name the name of the group in the partition, for the plot.
#' @param remove_outliers logical, to only plot data between quantile 0.1
#'  and 0.9 (FALSE by default)
#'
#' @export
#'
plot_clinical_features <- function(clinical_feature, partition,
                                   dot = FALSE, remove_outliers = FALSE,
                                   feature_name = "clinical_feature",
                                   group_name = "group") {
  df <- data_frame(
    clinical_feature = clinical_feature,
    group = 1:length(partition)
  )
  for (i in 1:length(partition)) {
    df$group[i] <- partition[i]
  }
  df$group <- as.factor(df$group)

  m <- ggplot2::ggplot(df) + ggplot2::aes(
    x = group, y = clinical_feature,
    fill = group
  )

  if (dot) {
    # hide the outliers drawn by the box plot:
    m <- m + ggplot2::geom_boxplot(outlier.size = 0, outlier.colour = "white")
    m <- m + ggplot2::geom_jitter(
      shape = 16,
      position = ggplot2::position_jitter(0.1)
    )
  } else {
    m <- m + ggplot2::geom_boxplot(
      outlier.colour = "black", outlier.shape = i,
      outlier.size = 2, notch = FALSE
    )
  }
  if (remove_outliers) {
    m <- m + ggplot2::coord_cartesian(ylim = quantile(
      x = df$clinical_feature,
      probs = c(0.1, 0.9)
    ))
  }
  m <- m + ggplot2::scale_color_brewer(palette = "Pastel1")
  plot(m, xlab = feature_name, ylab = group_name)
}


#' Plot clustered affinity matrix
#'
#' To be used with SNF and ANF affinity matrices for example.
#'
#' @param affinity_matrix An affinity matrix.
#' @param partition A partition of the samples.
#'
#' @export
#'
plot_affinity_matrix <- function(affinity_matrix, partition){
  # Reorder the distance matrix according to the partition:
  ind <- sort(as.vector(partition), index.return = TRUE)
  ind <- ind$ix
  # Normalize for a good display:
  diag(affinity_matrix) <- 0
  affinity_matrix <- affinity_matrix/rowSums(affinity_matrix)
  # Plot:
  image(1:ncol(affinity_matrix), 1:nrow(affinity_matrix),
        affinity_matrix[ind, ind], col = grey(100:0/100),
        xlab = "Samples", ylab = "Samples")
}
