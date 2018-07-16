
library(iClusterPlus)
iClusterPlus::iClusterBayes

load("data/data.RData")
datasets <- lapply(datasets, t)

t <- system.time(fit <- iClusterBayes(dt1 = datasets[[1]],
                    dt2 = datasets[[2]],
                    type = c("gaussian", "gaussian"),
                    K = 4 - 1))
# > t
# user  system elapsed
# 374.204   8.584 383.116
# 6 minutes ...
res <- list(partition = fit$clusters)
nmi <- subtypr::GetMetric("nmi")
nmi$Metric(res, real.groundtruth)
# > nmi$Metric(res, real.groundtruth)
# [1] 0.254498
# really bad because the model is not tuned...
