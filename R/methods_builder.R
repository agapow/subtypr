# Romain GUEDON
# methods.R

# script to create the methods.RData that will contain every formated methods.

#### Methods building ####

methods.list <- list()

#### PINSPlus ####

pins <- list(name = "pins", label = "PINSPlus")
formals(PINSPlus::SubtypingOmicsData)
pins$Func <- function(data.list,
                      return_stage_2 = TRUE,
                      kMax = 5,
                      agreementCutoff = 0.5,
                      verbose = T, ...) {

  result <- PINSPlus::SubtypingOmicsData(dataList = data.list,
                                         kMax = kMax,
                                         agreementCutoff = agreementCutoff,
                                         verbose = verbose,
                                         ... = ...)

  list(partition = if (return_stage_2) result$cluster2 else result$cluster1,
       data.returned = NULL)
}


#### SNF ####


snf <- list()
snf$name <- "snf"
snf$label <- "Similarity Network Fusion"

#the arguments are always: data.list and then method-specific arguments
snf$Func <- function(data.list, cluster.number,
                     K = 20,
                     alpha = 0.5,
                     t = 20,
                     spectral.clust.type = 3) {
  # fusion
  affinity.fused <- SNFtool::SNF(Wall = lapply(lapply(data.list, function(data) SNFtool::dist2(data, data)),
                                               function(dist) SNFtool::affinityMatrix(diff = dist,
                                                                                   K = K,
                                                                                   sigma = alpha)),
                                 K    = K,
                                 t    = t)
  # clustering
  partition <- SNFtool::spectralClustering(affinity = affinity.fused,
                                           K = cluster.number,
                                           type = spectral.clust.type)
  return(list("partition"     = partition,
              "data.returned" = affinity.fused))
}


methods.list$snf <- snf

#### ANF ####

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


#### Export methods.list ####

# make sure to have the good working dir
save(... = methods.list, file = "./inst/methods/methods.RData")
