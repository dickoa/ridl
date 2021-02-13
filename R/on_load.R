ridl_cache <- NULL # nocov start

#' @noRd
#' @importFrom hoardr hoard
.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("ridl")
  ridl_cache <<- x
  set_ridl_config()
} # nocov end
