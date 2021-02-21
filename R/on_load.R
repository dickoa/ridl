ridl_cache <- NULL # nocov start

#' @noRd
#' @importFrom hoardr hoard
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("ridl")
  ridl_cache <<- x
  .ridl_cm <<- cachem::cache_mem()
  search_ridl_dataset <<- memoise::memoise(search_ridl_dataset,
                                           cache = .ridl_cm)
  search_ridl_resource <<- memoise::memoise(search_ridl_resource,
                                            cache = .ridl_cm)
  pull_ridl_dataset <<- memoise::memoise(pull_ridl_dataset,
                                         cache = .ridl_cm)
  pull_ridl_resource <<- memoise::memoise(pull_ridl_resource,
                                          cache = .ridl_cm)
  pull_ridl_container <<- memoise::memoise(pull_ridl_container,
                                           cache = .ridl_cm)
  list_ridl_dataset.default <<- memoise::memoise(list_ridl_dataset.default,
                                                 cache = .ridl_cm)
  list_ridl_container.default <<- memoise::memoise(list_ridl_container.default,
                                                   cache = .ridl_cm)
  set_ridl_config()
} # nocov end
