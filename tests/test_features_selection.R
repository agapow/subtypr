library(subtypr)

data_list <- subtypr::gbm_data$data_list

check_distribution(data_list$methylation, thickness = 0.003, split_figures = TRUE)

pca_x <- select_features(data_list$methylation,
                         method = "pca",
                         min_pve = 0.8,
                         scale = TRUE)


var_x <- select_features(data_list$methylation, method = "variance", cutoff = 0.04)

mad_x <- select_features(data_list$methylation, method = "mad", cutoff = 0.1)



