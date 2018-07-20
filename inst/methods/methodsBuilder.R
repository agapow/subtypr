# Romain GUEDON
# methods.R

# script to create the methods.RData that will contain every formated methods.

#### Methods building ####

methods.list <- list()

#--- SNF


snf <- list()
snf$name <- "snf"
snf$label <- "Similarity Network Fusion"

#the arguments are always: data.list and then method-specific arguments
snf$Func <- function(data.list, cluster.number, K = 20, alpha = 0.5, t = 20, spectral.clust.type = 3) {
  # fusion
  affinity.fused <- SNFtool::SNF(Wall = lapply(lapply(data.list, function(d) SNFtool::dist2(d, d)),
                                      function(d) SNFtool::affinityMatrix(diff = d, K = K, sigma = alpha)),
                        K    = K,
                        t    = t)
  # clustering
  partition <- SNFtool::spectralClustering(affinity = affinity.fused, K = cluster.number, type = spectral.clust.type)
  return(list("partition"     = partition,
              "data.returned" = affinity.fused))
}


methods.list$snf <- snf

#--- ANF

anf <- list()
anf$name <- "anf"
anf$label <- "Affinity Network Fusion"

formals(ANF::spectral_clustering)

anf$Func <- function(data.list,
                     cluster.number,
                     k.affi,
                     alpha.affi = 1/6,
                     beta.affi = 1/6,
                     K.fusion = 20,
                     weigth.fusion = NULL,
                     type.fusion = c("two-step", "one-step"),
                     alpha.fusion = c(1, 1, 0, 0, 0, 0, 0, 0),
                     spectral.type = c("rw", "sym", "unnormalized"),
                     verbose.fusion = FALSE) {

  type.fusion = match.arg(type.fusion)
  spectral.type = match.arg(spectral.type)

  affinity.list <- lapply(data.list, function(data) ANF::affinity_matrix(D = as.matrix(dist(data)),
                                                                         k = k.affi,
                                                                         alpha = alpha.affi,
                                                                         beta = beta.affi))

  affinity.fused <- ANF::ANF(Wall = affinity.list, K = K.fusion, weight = weigth.fusion,
                             type = type.fusion, alpha = alpha.fusion, verbose = verbose.fusion)

  partition <- ANF::spectral_clustering(A = affinity.fused, k = cluster.number, type = spectral.type)

  return(list("partition"     = partition,
              "data.returned" = affinity.fused))
}

methods.list$anf <- anf

#### Tests ####
# names(methods.list)

#### Export ####

# make sure to have the good working dir
save(... = methods.list, file = "./inst/methods/methods.RData")
