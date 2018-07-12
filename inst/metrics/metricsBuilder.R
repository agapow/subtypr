# Romain GUEDON
# metrics.R

# generate the metrics.RData
# so it builds the object metrics.list


#### Metrics building ####

metrics.list <- list()



# Average Silhouette Width


# for affinity matrix:

aws.affinity <- list()

aws.affinity$label <- "Average silhouette width for affinity matrix"

aws.affinity$Metric <- function(res, ground.truth = NULL, plot = F) {
  if (!plot) {
    return(summary(CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned))$avg.width)
  } else {
    silx <- CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned)
    plot(silx)
  }
}

aws.affinity$maximize <- TRUE

metrics.list$aws.affinity <- aws.affinity

# Adjusted Rand Index

ari <- list()
ari$label <- "Adjusted rand index"
ari$Metric <- function(res, ground.truth, plot = F) {
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$corrected.rand
}

ari$maximize <- TRUE
metrics.list$ari <- ari


# Normalized Mutual Information

nmi <- list()
nmi$label <- "Normalized mutual information"
nmi$Metric <- function(res, ground.truth, plot = F) {
  SNFtool::calNMI(x = res$partition, y = ground.truth)
}
nmi$maximize <- TRUE

metrics.list$nmi <- nmi

# Meila's variation index VI

meilaVI <- list()
meilaVI$label <- "Meila's variation index VI"
meilaVI$Metric <- function(res, ground.truth, plot = F) {
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$vi
}

meilaVI$maximize <- TRUE

metrics.list$meilaVI <- meilaVI

save(... = metrics.list, file = "./inst/metrics/metrics.RData")




