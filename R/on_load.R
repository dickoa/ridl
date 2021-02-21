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
  search_datasets <<- memoise::memoise(search_datasets,
                                       cache = .ridl_cm)
  search_resources <<- memoise::memoise(search_resources,
                                       cache = .ridl_cm)
  pull_dataset <<- memoise::memoise(pull_dataset,
                                       cache = .ridl_cm)
  pull_resource <<- memoise::memoise(pull_resource,
                                       cache = .ridl_cm)
  pull_container <<- memoise::memoise(pull_container,
                                       cache = .ridl_cm)
  list_datasets.default <<- memoise::memoise(list_datasets.default,
                                             cache = .ridl_cm)
  list_containers.default <<- memoise::memoise(list_containers.default,
                                               cache = .ridl_cm)
  set_ridl_config()
} # nocov end
