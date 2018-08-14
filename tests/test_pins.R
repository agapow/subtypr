
?subtype_pins

pinx <- subtype_pins(breast_cancer_data$data_list, minimal_return = FALSE, k_max = 7, verbose = T, return_stage_2 = F)
plot(pinx$data_type_result$methylation$cluster)

overview_metrics(pinx, true_partition = breast_cancer_data$partition)

