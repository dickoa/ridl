.ridl_cache <- NULL # nocov start
.ridl_cm <- NULL

#' @noRd
#' @importFrom hoardr hoard
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @importFrom jsonlite fromJSON
.onLoad <- function(libname, pkgname) {# nolint

  x <- hoard()
  x$cache_path_set("ridl")
  .ridl_cache <<- x
  .ridl_cm <<- cache_mem()
  ridl_dataset_search <<- memoise(ridl_dataset_search,
                                    cache = .ridl_cm)
  ridl_resource_search <<- memoise(ridl_resource_search,
                                   cache = .ridl_cm)
  ridl_dataset_show <<- memoise(ridl_dataset_show,
                                         cache = .ridl_cm)
  ridl_resource_show <<- memoise(ridl_resource_show,
                                          cache = .ridl_cm)
  ridl_container_show <<- memoise(ridl_container_show,
                                           cache = .ridl_cm)
  ridl_dataset_list.default <<- memoise(ridl_dataset_list.default,
                                                 cache = .ridl_cm)
  ridl_container_list.default <<- memoise(ridl_container_list.default,
                                          cache = .ridl_cm)
  ridl_config_set()
}

## .onAttach <- function(libname, pkgname) { # nolint
##   if (!nzchar(Sys.getenv("RIDL_LOG"))) {
##     packageStartupMessage(
##       "<Logging NOT enabled: please install the logger package>")
##   } else {
##     packageStartupMessage(
##       sprintf("<Logging enabled: %s>",
##               Sys.getenv("RIDL_LOG")))
##   }
## } # nocov end