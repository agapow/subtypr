

tunex <- tuning(
  data_list = breast_cancer_data$data_list,
  method = "snf",
  grid_support = list(cluster_number = 4),
  metric = "asw_affinity",
  true_partition = NULL,
  parallel = F
)


affi <- SNFtool::affinityMatrix(SNFtool::dist2(
  breast_cancer_data$data_list[[1]],
  breast_cancer_data$data_list[[1]]
))
partition <- breast_cancer_data$partition


asw <- get_metric("asw_affinity")
get <- asw$metric(pred_partition = breast_cancer_data$partition, data_for_metric = affi)


sil <- silhouette_affinity(pred_partition = breast_cancer_data$partition, affinity_matrix = affi)
class(sil)


summary(object = silhouette_affinity(pred_partition = breast_cancer_data$partition, affinity_matrix = affi))$avg.width
