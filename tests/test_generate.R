library(SNFtool)
library(subtypr)

snf <- get_method("snf")

# Check partitions:
partitions <- generate_partitions(610, 3)
for (i in 1:3) {
  plot(partitions$partitions[i, ])
}


for (i in 1:3) {
  print(dim(svd(breast_cancer_data$data_list[[2]])[[i]]))
}

# Check generation:
multi <- generate_multi_structured(
  data_support = breast_cancer_data$data_list,
  structure_type = "moclus",
  sd_signal = 10,
  sparse = TRUE,
  percent_sparsity = c(0.5, 0, 0.5)
)





# Check distribution of simulated data, compared to original one:
check_distribution(multi[[1]][[1]])
check_distribution(breast_cancer_data$data_list[[1]])



snfx <- snf(data_list = multi$data_generated, cluster_number = 4, K = 10, alpha = 0.3, t = 10)


plot_affinity_matrix(snfx$affinity_fused, snfx$partition)

normalized_mutual_information(snfx$partition, multi$partition_tot)
