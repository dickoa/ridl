.ridl_cache <- NULL # nocov start
.ridl_cm <- NULL

#' @noRd
#' @importFrom hoardr hoard
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @importFrom jsonlite fromJSON
.onLoad <- function(libname, pkgname) {
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
  ## ridl_dataset_schema_url <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/dataset.json"
  ## .ridl_dataset_schema <<- fromJSON(ridl_dataset_schema_url,
  ##                                             simplifyVector = FALSE)

  ## ridl_container_schema_url <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/data_container.json"
  ## .ridl_container_schema <<- fromJSON(ridl_container_schema_url,
  ##                                               simplifyVector = FALSE)
  ridl_dataset_schema_file <- system.file("schemas/dataset.json",
                                          package = "ridl")
  .ridl_dataset_schema <<- fromJSON(ridl_dataset_schema_file,
                                    simplifyVector = FALSE)
  ridl_container_schema_file <- system.file("schemas/data_container.json",
                                            package = "ridl")
  .ridl_container_schema <<- fromJSON(ridl_container_schema_file,
                                      simplifyVector = FALSE)

  ridl_config_set()
} # nocov end
