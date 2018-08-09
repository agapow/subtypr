snf <- get_method("snf")
snfx <- snf(data_list = breast_cancer_data$data_list, minimal_return = F, cluster_number = 4)
plot_affinity_matrix(snfx$affinity_fused, partition = snfx$partition)
