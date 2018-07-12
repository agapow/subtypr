


library(subtypr)


load("data/data.RData")

snf <- GetMethodInfo("snf")
asw <- GetMetricInfo()[[1]]
grid.support <- list(K = 5:6, alpha = 0.5, cluster.number = 2)

data.list <- lapply(datasets, t)

tuneMe <- Tuning(data.list = data.list, method = snf, grid.support = grid.support, metric = asw, parallel = TRUE, plot = TRUE)

