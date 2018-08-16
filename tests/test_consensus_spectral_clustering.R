library(subtypr)
snfx <-subtype_snf(breast_cancer_data$data_list, cluster_number = 4)

cc <-consensus_spectral_clustering(
  affinity_matrix = snfx$affinity_fused,
  cluster_number_max = 5
)
