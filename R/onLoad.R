

# # global references to python modules (will be initialized in .onLoad)
# tskit <- NULL
# pyslim <- NULL
# msprime <- NULL
# np <- NULL
# pd <- NULL
#
# .onLoad <- function(libname, pkgname) {
#   # use superassignment to update global reference
#   tskit <<- reticulate::import("tskit", delay_load = TRUE)
#   pyslim <<- reticulate::import("pyslim", delay_load = TRUE)
#   msprime <<- reticulate::import("msprime", delay_load = TRUE)
#   np <<- reticulate::import("numpy", delay_load = TRUE)
#   pd <<- reticulate::import("pandas", delay_load = TRUE)
# }
