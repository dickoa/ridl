#' RIDLObject abstract class
#'
#' RIDLObject class containing all logic for accessing,
#' creating, and updating RIDL objects.
RIDLObject <- R6::R6Class(
  classname = "RIDLObject",
  private = list(
    configuration = NULL
  ),
  public = list(
    #' @field data placeholder for RIDLObject field element
    data = list(),
    #' @description
    #' Create a new RIDLObject object
    #'
    #' @param initial_data list with required field to create a RIDLObject
    #' @param configuration a Configuration object
    #' @return A RIDLObject object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- get_ridl_config()
      } else {
        private$configuration <- configuration
      }
    },

    #' @description
    #' Get RIDLObject field into list
    #'
    #' @return a list with RIDLObject field
    as_list = function() {
      self$data
    },

    #' @description
    #' Browse RIDL
    browse = function() {
      url <- private$configuration$get_ridl_site_url()
      browseURL(url = url)
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    get_configuration = function() {
      private$configuration
    },

    #' @description
    #' Print a Dataset object
    print = function() {
      cat(paste0("<RIDL Object> ", self$data$id), "\n")
      cat("This is an abstract class!\n")
      invisible(self)
    }
  )
)

#' @rdname tag_names
#' @export
tag_names <- function(dataset)
  UseMethod("tag_names")

#' @rdname tag_names
#' @export
tag_names.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname list_ridl_dataset
#' @export
list_ridl_dataset <- function(container, configuration)
  UseMethod("list_ridl_dataset")

#' @rdname list_ridl_container
#' @export
list_ridl_container <- function(sort, configuration)
  UseMethod("list_ridl_container")

#' @rdname get_format
#' @export
get_format <- function(resource)
  UseMethod("get_format")

#' @rdname get_format
#' @export
get_format.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname get_sheets
#' @export
get_sheets <- function(resource, format, download_folder, quiet)
  UseMethod("get_sheets")

#' @rdname get_sheets
#' @export
get_sheets.default <- function(resource, format, download_folder, quiet) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname get_dataset
#' @export
get_ridl_dataset <- function(resource)
  UseMethod("get_ridl_dataset")

#' @rdname get_dataset
#' @export
get_ridl_dataset.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname get_ridl_resource
#' @export
get_ridl_resource <- function(dataset, n)
  UseMethod("get_ridl_resource")

#' @rdname get_ridl_resource
#' @export
get_ridl_resource.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname list_ridl_resource
#' @export
list_ridl_resource <- function(dataset, pattern, format)
  UseMethod("list_ridl_resource")

#' @rdname list_ridl_resource
#' @export
list_ridl_resource.default <- function(dataset, pattern, format) {
    stop("Only available for a RIDLDataset object",
         call. = TRUE)
}

#' @rdname delete_ridl_resource
#' @export
delete_ridl_resource <- function(dataset, n)
  UseMethod("delete_ridl_resource")

#' @rdname delete_ridl_resource
#' @export
delete_ridl_resource.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname delete_all_ridl_resource
#' @export
delete_all_ridl_resource <- function(dataset)
  UseMethod("delete_all_ridl_resource")

#' @rdname delete_all_ridl_resource
#' @export
delete_all_ridl_resource.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname add_ridl_resource
#' @export
add_ridl_resource <- function(dataset, resource, ignore_dataset_id, configuration)
  UseMethod("add_ridl_resource")

#' @rdname add_ridl_resource
#' @export
add_ridl_resource.default <- function(dataset, resource, ignore_dataset_id, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname set_ridl_container
#' @export
set_ridl_container <- function(dataset, container_name, configuration)
  UseMethod("set_ridl_container")

#' @rdname set_ridl_container
#' @export
set_ridl_container.default <- function(dataset, container_name, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname get_ridl_container
#' @export
get_ridl_container <- function(dataset)
  UseMethod("get_ridl_container")

#' @rdname get_ridl_container
#' @export
get_ridl_container.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname n_ridl_resource
#' @export
n_ridl_resource <- function(dataset)
  UseMethod("n_ridl_resource")

#' @rdname n_ridl_resource
#' @export
n_ridl_resource.default <- function(dataset) {
  stop("It only works with RIDLDataset object",
       call. = TRUE)
}

#' @rdname read
#' @export
read <- function(resource, sheet, format,
                 download_folder, force_download,
                 quiet_download, ...)
  UseMethod("read")

#' @rdname read
#' @export
read.default <- function(resource, sheet, format,
                         download_folder, force_download,
                         quiet_download, ...) {
  stop("It only works with RIDLResource object",
       call. = TRUE)
}

#' @rdname download
#' @export
download <- function(resource, folder, filename, quiet, force, ...)
  UseMethod("download")

#' @rdname download
#' @export
download.default <- function(resource, folder, filename, quiet, force, ...) {
  stop("It only works with RIDLResource object",
       call. = TRUE)
}
