# Generates the metrics.RData so it builds the object metrics_list

#### Documentation of metrics ####

#' Metrics implemented
#'
#' subtypr provide both internal and external metrics to evaluate clustering
#'   results.
#'
#' There is two versions of each metric. The raw version - directly accessible
#' by the user - and a standardized version for the built-in functions of the
#' package (the idea is that the result is numeric, e.g. for silhouette index,
#' the value is the average silhouette width). The argument `metric` is always
#' the name of the standardized version.
#' You can access the standardized version with the
#' `get_metric` function. To have all the names of the standardized metrics,
#'  use `names(get_metric())`.
#'
#' Here we indicate the raw version documentation name and it's corresponding
#' standardized version: like this:
#' ?raw_version ~ "name_of_standardized_version"
#'
#' Internal metrics:
#'
#'   * Silhouette index: ?silhouette_distance ~ "asw_distance" or
#'   ?silhouette_affinity ~ "asw_affinity"
#'
#' External metrics:
#'   * Adjusted Rand Index: ?adjusted_rand_index ~ "ari"
#'   * Mutual Information: ?mutual_information ~ "mi"
#'   * Normalized Mutual Information: ?normalized_mutual_information ~ "nmi"
#'   * Meila's Variation Index: ?meila_vi ~ "meilaVI"
#'   * Dunn index: ?dunn_index ~ "dunn"
#'   * Homogeneity, Completeness & V-measure:
#'   ?homogeneity_completeness_vmeasure ~ "homogeneity" or "completeness" or
#'   "v_measure"
#'
"metrics_list"


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
    silhouette_affinity(
      pred_partition = pred_partition,
      affinity_matrix = data_for_metric,
      average = TRUE
    )
  }
)





# for distance matrix:

metrics_list$asw_distance <- list(
  name = "asw_distance",
  label = "Average silhouette width (distance matrix)",
  maximize = TRUE,
  internal = TRUE,
  metric = function(pred_partition, true_partition, data_for_metric) {
    silhouette_distance(
      pred_partition = pred_partition,
      distance_matrix = data_for_metric,
      average = TRUE
    )
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

# #### Cox p-value ####
# metrics_list$coxpval <- list(
#   name = "coxpval",
#   label = "Cox p-value",
#   maximize = TRUE,
#   internal = TRUE,
#   metric = function(pred_partition, true_partition = NULL, data_for_metric) {
#     analyze_survival(survival_time = data_for_metric$survival_time,
#                      death_status = data_for_metric$death_status,
#                      patient_partition = pred_partition,
#                      return = TRUE,
#                      no_plot = TRUE)
#   }
# )

#### Export metrics_list ####

save(... = metrics_list, file = "./inst/metrics/metrics.RData")
