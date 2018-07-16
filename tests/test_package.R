#test_package.R

library(subtypr)

grid.support <- list(K = 5, alpha = 0.5, cluster.number = 2)

data.list <- lapply(datasets, t)

grid.support <- list(cluster.number = 4,
                     k.affi = 5,
                     K.fusion = 5)

tuning.result <- Tuning(data.list = datasets, method = "anf",
                        grid.support = grid.support, metric = "nmi",
                        ground.truth = real.groundtruth, parallel = TRUE, plot = TRUE)


MetricValues()





