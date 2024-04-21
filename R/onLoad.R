

# # global references to python modules (will be initialized in .onLoad)


.onLoad <- function(libname, pkgname) {
  reticulate::use_condaenv(condaenv = Sys.getenv("MUP_CONDA"))
}
