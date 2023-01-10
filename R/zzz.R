.onAttach <- function(libname, pkgname) {
  if (requireNamespace("parallel", quietly = TRUE) & !exists(".ctx")) {
    packageStartupMessage("* Registering parallel context")
    .ctx <<- parallel::makeCluster(parallel::detectCores() - 2)
  }
}

.onDetach <- function(libpath) {
  if (exists(".ctx")) {
    parallel::stopCluster(.ctx)
  }
}
