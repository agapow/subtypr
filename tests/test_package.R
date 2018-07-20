#test_package.R

library(subtypr)

data.support <- lapply(datasets, t)
snf <- GetMethod("snf")$Func
anf <- GetMethod("anf")$Func


data.synth <- synth_based_on_data(data.support = data.support, structure.type = "basic", separation = 5)
data <- data.synth$data.list

#shuffle
permut <- sample(608)
shuffled.data <- lapply(data, function(X) X[permut, ])
shuffled.partition <- data.synth$partition[permut]

snfx.shuffled <- snf(data.list = shuffled.data, cluster.number = 4, K = 10, alpha = 0.5, t = 20)
displayClusters(snfx.shuffled$data.returned, snfx.shuffled$partition)
calNMI(snfx.shuffled$partition, shuffled.partition)


#normal
snfx <- snf(data.list = data, cluster.number = 4, K = 10, alpha = 0.5, t = 20)
displayClusters(snfx$data.returned, snfx$partition)
calNMI(snfx$partition, data.synth$partition)


# any difference ?

calNMI(snfx$partition[permut], snfx.shuffled$partition) #no !


for (i in 1:3) {
  distx <- dist2(X = data[[i]], C = data[[i]])
  affix <- affinityMatrix(distx)
  displayClusters(affix, rep(1,608))
}



snfx <- snf(data.list = data.synth$data.list, cluster.number = 4, K = 10, alpha = 0.5, t = 20)
displayClusters(snfx$data.returned, snfx$partition)
calNMI(snfx$partition, data.synth$partition)

anfx <- anf(data.list = data, cluster.number = 4, k.affi = 20, type.fusion = "dssd")
displayClusters(anfx$data.returned, anfx$partition)
calNMI(anfx$partition, data.synth$partition)


one_data <- generate_synth_data("gaussian", c(100,1000))

CancerSubtypes::data.checkDistribution(t(one_data))

