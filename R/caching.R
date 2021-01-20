#' Caching RIDL downloaded files
#'
#' Manage cached RIDL downloaded files
#'
#' @name ridl_cache
#'
#' @details The default cache directory is
#' `~/.cache/R/ridl_cache`, but you can set
#' your own path using `ridl_cache_set_dir()`
#'
#'
#' @examples \dontrun{
#' ridl_cache
#' ## change the default cache directory
#' tmp <- tempdir()
#' ridl_cache_set_dir(tmp)
#'
#' ## print current cache directory
#' ridl_cache_get_dir()
#'
#' ## List available files in the current cache directory
#' ridl_cache_list()
#'
#' l <- ridl_cache_list()[1] ## get the first file
#' ridl_cache_delete(l) ## delete it
#'
#' ridl_cache_clear() ## delete all cached files
#' }
NULL

#' Set the cache directory
#'
#' @rdname ridl_cache
#'
#' @param path Character directory to set
#'
#' @return the cache directory
#' @export
ridl_cache_set_dir <- function(path) {
  assert_cache(ridl_cache)
  ridl_cache$cache_path_set(path)
}

#' Print the cache directory
#'
#' @rdname ridl_cache
#'
#' @return the cache directory
#' @export
ridl_cache_get_dir <- function() {
  assert_cache(ridl_cache)
  ridl_cache$cache_path_get()
}

#' List of files available in the cache directory
#'
#' @rdname ridl_cache
#'
#' @return list of files in the cache
#' @export
ridl_cache_list <- function() {
  assert_cache(ridl_cache)
  list.files(ridl_cache$cache_path_get())
}

#' Delete a given file from cache
#'
#' @rdname ridl_cache
#'
#' @param file Character, the file to delete
#'
#' @export
ridl_cache_delete <- function(file) {
  assert_cache(ridl_cache)
  ridl_cache$delete(file)
}

#' Clear cache directory
#'
#' @rdname ridl_cache
#'
#' @export
ridl_cache_clear <- function() {
  assert_cache(ridl_cache)
  ridl_cache$delete_all()
}
