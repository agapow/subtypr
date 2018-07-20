# Romain GUEDON
# metrics.R

# generate the metrics.RData
# so it builds the object metrics.list

#### Create metrics.list ####


metrics.list <- list()



#### Average Silhouette Width ####


# for affinity matrix:

asw.affinity <- list()
asw.affinity$name <- "asw.affinity"
asw.affinity$label <- "Average silhouette width (affinity matrix)"

asw.affinity$Metric <- function(res, ground.truth = NULL, plot = F) {
  if (!plot) {
    return(summary(CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned))$avg.width)
  } else {
    silx <- CancerSubtypes::silhouette_SimilarityMatrix(group = res$partition, similarity_matrix = res$data.returned)
    plot(silx)
    return(summary(silx)$avg.width)
  }
}

asw.affinity$maximize <- TRUE

metrics.list$asw.affinity <- asw.affinity

#### Adjusted Rand Index ####

ari <- list()
ari$name <- "ari"
ari$label <- "Adjusted rand index"
ari$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$corrected.rand
}

ari$maximize <- TRUE
metrics.list$ari <- ari


#### Normalized Mutual Information ####

nmi <- list()
nmi$name <- "nmi"
nmi$label <- "Normalized mutual information"
nmi$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  SNFtool::calNMI(x = res$partition, y = ground.truth)
}
nmi$maximize <- TRUE

metrics.list$nmi <- nmi

#### Meila's variation index VI ####

meilaVI <- list()
meilaVI$name <- "meilaVI"
meilaVI$label <- "Meila's variation index VI"
meilaVI$Metric <- function(res, ground.truth, plot = F) {
  if (is.null(ground.truth)) stop("ground.truth is missing")
  ground.truth <- as.integer(ground.truth)
  fpc::cluster.stats(d = NULL, clustering = res$partition, alt.clustering = ground.truth, compareonly = T)$vi
}

meilaVI$maximize <- TRUE

metrics.list$meilaVI <- meilaVI


#### Mutual Information ####

mi <- list(name = "mi", label = "Mutual Information", maximize = TRUE)


mi$Metric <- function(res, ground.truth, plot = F) {
  mutual_information(res$partition, ground.truth)
}

metrics.list$mi <- mi

#### Homogeneity ####

homogeneity <- list(name = "homogeneity", label = "Homogeneity", maximize = TRUE)

homogeneity$Metric <- function(res, ground.truth, plot= F) {
  homogeneity_completeness_vmeasure(ground.truth, res$partition)$homogeneity
}

metrics.list$homogeneity <- homogeneity

#### Completeness ####

completeness <- list(name = "completeness", label = "Completeness", maximize = TRUE)
completeness$Metric <- function(res, ground.truth, plot= F) {
  homogeneity_completeness_vmeasure(ground.truth, res$partition)$completeness
}

metrics.list$completeness <- completeness
#### V_measure ####

v_measure <- list(name = "v_measure", label = "V_measure", maximize = TRUE)
v_measure$Metric <- function(res, ground.truth, plot= F) {
  homogeneity_completeness_vmeasure(ground.truth, res$partition)$vmeasure
}

metrics.list$v_measure <- v_measure

#### Export metrics.list ####

save(... = metrics.list, file = "./inst/metrics/metrics.RData")

