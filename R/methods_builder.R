# Romain GUEDON
# methods.R

# script to create the methods.RData that will contain every formated methods.

#### Methods building ####

methods_list <- list(
  subtype_pins = subtype_pins,
  subtype_anf = subtype_anf,
  subtype_snf = subtype_snf
)


#### Export methods_list ####

# make sure to have the good working dir:
save(... = methods_list, file = "./inst/methods/methods.RData")
