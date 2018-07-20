# for visualising clusering results - hair-and-whisker plots?

ClinicalBoxplot <- function(clinical.feature, partition, dot = FALSE,
                            feature.name = "clinical.feature",
                            group.name = "group") {

  df <- data.frame(clinical.feature = clinical.feature, group = 1:length(partition))
  for (i in 1:length(partition)) {
    df$group[i] <- partition[i]
  }
  df$group <- as.factor(df$group)

  m <- ggplot2::ggplot(df) + ggplot2::aes(x = group, y = clinical.feature, fill = group)

  if (dot) {
    m <- m + ggplot2::geom_boxplot(outlier.size = 0, outlier.colour = "white")
    m <- m + ggplot2::geom_jitter(shape = 16, position = ggplot2::position_jitter(0.1))
  } else {
    m <- m + ggplot2::geom_boxplot(outlier.colour = "black", outlier.shape = i,
                                   outlier.size = 2, notch = FALSE)
  }
  m <- m + ggplot2::scale_color_brewer(palette = "Pastel1")
  plot(m,, xlab = feature.name, ylab = group.name)
}


# dummy.dose <- c(rnorm(100), rnorm(100, 4), rnorm(100, 6, 3), 40)
# dummy.partition <- c(rep(1, 100), rep(2, 100), rep(3, 101))
# ClinicalBoxplot(clinical.feature = dummy.dose, partition = dummy.partition, dot = TRUE)
