# Romain GUEDON
# methods.R

# script to create the methods.RData that will contain every formated methods.

#### Methods building ####

methods.list <- list()

#--- SNF

#init
snf <- list()

#label
snf$label <- "Similarity Network Fusion"

#FuncStandard
snf$Func <- function(data.list, cluster.number, K = 20, alpha = 0.5, t = 20, spectral.clust.type = 3) {
  # fusion
  affinity.fused <- SNFtool::SNF(Wall = lapply(lapply(data.list, function(d) SNFtool::dist2(d, d)),
                                      function(d) SNFtool::affinityMatrix(diff = d, K = K, sigma = alpha)),
                        K    = K,
                        t    = t)
  # clustering
  partition <- SNFtool::spectralClustering(affinity = affinity.fused, K = cluster.number, type = spectral.clust.type)
  return(list("partition"           = partition,
              "data.returned" = affinity.fused))
}


methods.list$snf <- snf


#### Tests ####
names(methods.list)

#### Export ####

# make sure to have the good working dir
save(... = methods.list, file = "./inst/methods/methods.RData")
