# Romain GUEDON
# metrics.R

# generate the metrics.RData
# so it builds the object metrics_list

#### Create metrics_list ####


metrics_list <- list()



#### Average Silhouette Width ####


# for affinity matrix:

metrics_list$asw_affinity <- list(
  name = "asw_affinity",
  label = "Average silhouette width (affinity matrix)",
  maximize = TRUE,
  internal = TRUE,
  metric = function(pred_partition, true_partition, data_for_metric) {
    silhouette_affinity(pred_partition = pred_partition,
                        affinity_matrix = data_for_metric,
                        average = TRUE)
  }
)





# for distance matrix:

metrics_list$asw_distance <- list(
  name = "asw_distance",
  label = "Average silhouette width (distance matrix)",
  maximize = TRUE,
  internal = TRUE,
  metric = function(pred_partition, true_partition, data_for_metric) {
    silhouette_distance(pred_partition = pred_partition,
                        distance_matrix = data_for_metric,
                        average = TRUE)
  }
)




#### Adjusted Rand Index ####

metrics_list$ari <- list(
  name = "ari",
  label = "Adjusted rand index",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    adjusted_rand_index(
      pred_partition = pred_partition,
      true_partition = true_partition
    )
  }
)




#### Normalized Mutual Information ####

metrics_list$nmi <- list(
  name = "nmi",
  label = "Normalized mutual information",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    normalized_mutual_information(
      pred_partition = pred_partition,
      true_partition = true_partition
    )
  }
)




#### Meila's variation index VI ####

metrics_list$meilaVI <- list(
  name = "meilaVI",
  label = "Meila's variation index VI",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    meila_vi(
      pred_partition = pred_partition,
      true_partition = true_partition
    )
  }
)





#### Mutual Information ####

metrics_list$mi <- list(
  name = "mi",
  label = "Mutual Information",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    mutual_information(
      pred_partition = pred_partition,
      true_partition = true_partition
    )
  }
)




#### Homogeneity ####

metrics_list$homogeneity <- list(
  name = "homogeneity",
  label = "Homogeneity",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    homogeneity_completeness_vmeasure(
      pred_partition = pred_partition,
      true_partition = true_partition
    )$homogeneity
  }
)



#### Completeness ####

metrics_list$completeness <- list(
  name = "completeness",
  label = "Completeness",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    homogeneity_completeness_vmeasure(
      pred_partition = pred_partition,
      true_partition = true_partition
    )$completeness
  }
)



#### V_measure ####

metrics_list$v_measure <- list(
  name = "v_measure",
  label = "V_measure",
  maximize = TRUE,
  internal = FALSE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    homogeneity_completeness_vmeasure(
      pred_partition = pred_partition,
      true_partition = true_partition
    )$vmeasure
  }
)



#### Dunn Index ####
metrics_list$dunn <- list(
  name = "dunn",
  label = "Dunn index",
  maximize = TRUE,
  internal = TRUE,
  metric = function(pred_partition, true_partition, data_for_metric = NULL) {
    dunn_index(
      pred_partition = pred_partition,
      distance_matrix = data_for_metric
    )
  }
)


#### Export metrics_list ####

save(... = metrics_list, file = "./inst/metrics/metrics.RData")
