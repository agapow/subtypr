# Romain GUEDON
# methods.R

# script to create the methods.RData that will contain every formated methods.

#### Methods building ####

methods_list <- list(
  pins = subtype_pins,
  anf = subtype_anf,
  snf = subtype_snf
)


#### Export methods_list ####

# make sure to have the good working dir:
save(... = methods_list, file = "./inst/methods/methods.RData")
