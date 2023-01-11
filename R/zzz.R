
.onLoad <- function(libname, pkgname) {
  assign("pufr_env", new.env(), parent.env(environment()))
}

#' Register a parallel cluster
#'
#' The user should create the cluster and close it.
#'
#' @param ctx A cluster created with [parallel::makeCluster()]
#'
#' @export
#' @seealso [parallel::makeCluster()]
#' @examples
#' \dontrun{
#' ctx <- parallel::makeCluster(2)
#' register_parallel(ctx)
#' }
register_parallel <- function(ctx) {
  pufr_env$ctx <- ctx
}
