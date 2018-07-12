load("./data/data.RData")

grid.support <- list(K = 5, alpha = 0.5, cluster.number = 2)

data.list <- lapply(datasets, t)








# formals(GetMethodInfo("anf")$Func)
grid.support <- list(cluster.number = 2:3,
                     k.affi = 5,
                     K.fusion = 5)

tuning.result <- Tuning(data.list = data.list, method = "anf",
                        grid.support = grid.support, metric = "nmi",
                        ground.truth = real.groundtruth, parallel = FALSE, plot = TRUE)



# method <- GetMethodInfo("anf")
# all.args <- as.list(base::formals(method$Func))
# all.args[names(grid.support)] <- grid.support
#
# # remove data.list from the args
# i.data <- which(names(all.args) == "data.list")
# all.args <- all.args[-i.data]
# print("all.atgs = ")
# print(all.args)
#
# # span the grid
# grid <- base::expand.grid(all.args, stringsAsFactors = )
# l <- dim(grid)[1]
# print("grid  = ")
# print(grid)
#





