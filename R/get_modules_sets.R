#' Get All Environment Modules in a Module Path
#'
#' @param module_repositories ...
#'
#' @param \dots Additional arguments passed to [module_avail()].
#'
#' @return A list ...
#'
#' @export
get_modules_sets <- function(module_repositories, ...) {
  modules_sets <- lapply(names(module_repositories), FUN = function(name) {
    info <- module_repositories[[name]]
    modules <- module_avail(info, ...)
    if (!is.null(modules)) attr(modules, "repository") <- name
    modules
  })
  names(modules_sets) <- names(module_repositories)

  ## Drop empty sets
  modules_sets <- modules_sets[lengths(modules_sets) > 0]

  modules_sets
}
