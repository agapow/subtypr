#test_package.R

library(subtypr)
library(SNFtool)

data.support <- lapply(datasets, t)
snf <- GetMethod("snf")$Func
anf <- GetMethod("anf")$Func


data.synth <- synth_based_on_data(data.support = data.support, structure.type = "basic", separation = 5)
data <- data.synth$data.list

snfx <- snf(data.list = data, cluster.number = 4, K = 10, alpha = 0.5, t = 20)
displayClusters(snfx$data.returned, snfx$partition)
calNMI(snfx$partition, data.synth$partition)


for (i in 1:3) {
  distx <- dist2(X = data[[i]], C = data[[i]])
  affix <- affinityMatrix(distx)
  displayClusters(affix, rep(1,608))
}



snfx <- snf(data.list = data.synth$data.list, cluster.number = 4, K = 10, alpha = 0.5, t = 20)
displayClusters(snfx$data.returned, snfx$partition)
calNMI(snfx$partition, data.synth$partition)

one_data <- generate_synth_data("gaussian", c(100,1000))

CancerSubtypes::data.checkDistribution(t(one_data))

