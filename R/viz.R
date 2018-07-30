#' Plot the distribution of the features
#'
#' The function plots the distribution of the mean, the variance and the median absolute deviation
#'  of the features.
#'
#'
#' @param data a feature matrix with samples in rows and features in columns
#'
#' @export
#'
check_distribution <- function(data) {
  mean_features = apply(data, 2, function(x) mean(x, na.rm = TRUE))
  variance_features = apply(data, 2, function(x) var(x, na.rm = TRUE))
  MAD_features = apply(data, 2, function(x) mad(x, na.rm = TRUE))
  feature_num = ncol(data)
  par(mfrow = c(3, 1))
  hist(mean_features, breaks = feature_num * 0.1, col = "red",
       main = "data (mean) distribution", xlab = "Average value of features")
  hist(variance_features, breaks = feature_num * 0.1, col = "red",
       main = "data (Variance) distribution", xlab = "Variance of features")
  hist(MAD_features, breaks = feature_num * 0.1, col = "red",
       main = "data (MAD) distribution", xlab = "Median absolute deviation of features")
  par(mfrow = c(1, 1))
}




#' Box-plot of clinical features for a given partition of the samples.
#'
#' Plot the box-plot of the given clinical features, splitting data according to the given partition.
#'
#' @param clinical.feature a numeric vector with a value for each patients.
#' @param partition an integer (or factor) vector indicating the group of each patient.
#' @param dot logical, to plot the points on the box-plot
#' @param feature.name the name of the clinical feature plotted.
#' @param group.name the name of the group in the partition, for the plot.
#' @param remove.outliers logical, to only plot data between quantile 0.1 and 0.9 (FALSE by default)
#'
#' @export
#'
plot_clinical_features <- function(clinical.feature, partition, dot = FALSE, remove.outliers = FALSE,
                            feature.name = "clinical.feature",
                            group.name = "group") {

  df <- data.frame(clinical.feature = clinical.feature, group = 1:length(partition))
  for (i in 1:length(partition)) {
    df$group[i] <- partition[i]
  }
  df$group <- as.factor(df$group)

  m <- ggplot2::ggplot(df) + ggplot2::aes(x = group, y = clinical.feature, fill = group)

  if (dot) {
    m <- m + ggplot2::geom_boxplot(outlier.size = 0, outlier.colour = "white")# hide the outliers drawn by the box plot
    m <- m + ggplot2::geom_jitter(shape = 16, position = ggplot2::position_jitter(0.1))
  } else {
    m <- m + ggplot2::geom_boxplot(outlier.colour = "black", outlier.shape = i,
                                   outlier.size = 2, notch = FALSE)
  }
  if (remove.outliers) {m <- m + ggplot2::coord_cartesian(ylim = quantile(x = df$clinical.feature, probs = c(0.1, 0.9)))}
  m <- m + ggplot2::scale_color_brewer(palette = "Pastel1")
  plot(m, xlab = feature.name, ylab = group.name)
}


