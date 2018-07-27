
load("data/GBM.RData")

data.list <- gbm.data[c("gene.exp", "methy.exp", "mirna.exp")]

library(subtypr)
#### TEST ####

snf <- GetMethod("snf")
snfx <- snf$Func(data.list = data.list, cluster.number = 4, K = 20, alpha = 0.5, t = 20, spectral.clust.type = 3)


formals(GetMethod("snf")$Func)


grid.support.snf <- list(cluster.number = 2:7,
                         K = seq(5,25,2),
                         alpha = seq(0.3,0.8,0.1))

# tuning.snf <- Tuning(data.list = data.list,
#                      method = "snf",
#                      grid.support = grid.support.snf,
#                      metric = "asw.affinity",
#                      ground.truth = NULL,
#                      parallel = T,
#                      plot = T)


#save(tuning.snf, file = "./tests/tuning_snf.RData")

load("./tests/tuning_snf.RData")

MetricValues(method.result = tuning.snf$method.res,internal.metrics = "asw.affinity")


grid.support.anf <- list(cluster.number = 2,#2:7,
                         k.affi = 5, #seq(5,25,2),
                         alpha.affi = 0.05,#seq(0.05, 2, 0.5),
                         beta.affi = 0.05,#seq(0.05, 2, 0.5),
                         K.fusion = 13,#,seq(5,25,2),
                         type.fusion = "two-step")

tuning.anf <- Tuning(data.list = data.list,
                     method = "anf",
                     grid.support = grid.support.anf,
                     metric = "asw.affinity",
                     ground.truth = NULL,
                     parallel = F,
                     plot = T)


save(tuning.anf, file = "./tests/tuning_anf.RData")


MetricValues(tuning.result = tuning.anf, ground.truth = NULL, metrics.names = c("asw.affinity"))



