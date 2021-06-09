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
        private$configuration <- ridl_config_get()
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
    browse_on_ridl = function() {
      url <- private$configuration$get_site_url()
      browseURL(url = url)
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    get_config = function() {
      private$configuration
    },

    #' @description
    #' Get dataset fields
    #'
    #' @return list of fields for a dataset
    get_fields = function() {
    },

    #' @description
    #' Get dataset required fields
    #'
    #' @return list of required fields for a dataset
    get_required_fields = function() {
    },

    #' @description
    #' Check dataset required field
    #'
    #' @return a logical value, TRUE if the the dataset
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
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

#' @rdname ridl_dataset_list
#' @export
ridl_dataset_list <- function(container, configuration)
  UseMethod("ridl_dataset_list")

#' @rdname ridl_container_list
#' @export
ridl_container_list <- function(sort, user_container, configuration)
  UseMethod("ridl_container_list")

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

#' @rdname set_file_to_upload
#' @export
set_file_to_upload <- function(resource, file_to_upload)
  UseMethod("set_file_to_upload")

#' @rdname set_file_to_upload
#' @export
set_file_to_upload.default <- function(resource, file_to_upload) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname get_file_to_upload
#' @export
get_file_to_upload <- function(resource)
  UseMethod("get_file_to_upload")

#' @rdname get_file_to_upload
#' @export
get_file_to_upload.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_dataset_get
#' @export
ridl_dataset_get <- function(resource)
  UseMethod("ridl_dataset_get")

#' @rdname ridl_dataset_get
#' @export
ridl_dataset_get.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_get
#' @export
ridl_resource_get <- function(dataset, n)
  UseMethod("ridl_resource_get")

#' @rdname ridl_resource_get
#' @export
ridl_resource_get.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_resource_get_all
#' @export
ridl_resource_get_all <- function(dataset, pattern, format)
  UseMethod("ridl_resource_get_all")

#' @rdname ridl_resource_get_all
#' @export
ridl_resource_get_all.default <- function(dataset, pattern, format) {
    stop("Only available for a RIDLDataset object",
         call. = TRUE)
}

#' @rdname ridl_resource_delete
#' @export
ridl_resource_delete <- function(dataset, n)
  UseMethod("ridl_resource_delete")

#' @rdname ridl_resource_delete
#' @export
ridl_resource_delete.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_resource_delete_all
#' @export
ridl_resource_delete_all <- function(dataset)
  UseMethod("ridl_resource_delete_all")

#' @rdname ridl_resource_delete_all
#' @export
ridl_resource_delete_all.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_resource_add
#' @export
ridl_resource_add <- function(dataset, resource, ignore_dataset_id, configuration)
  UseMethod("ridl_resource_add")

#' @rdname ridl_resource_add
#' @export
ridl_resource_add.default <- function(dataset, resource, ignore_dataset_id, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_container_set
#' @export
ridl_container_set <- function(dataset, container_name, configuration)
  UseMethod("ridl_container_set")

#' @rdname ridl_container_set
#' @export
ridl_container_set.default <- function(dataset, container_name, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_container_get
#' @export
ridl_container_get <- function(dataset)
  UseMethod("ridl_container_get")

#' @rdname ridl_container_get
#' @export
ridl_container_get.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_resource_n
#' @export
ridl_resource_n <- function(dataset)
  UseMethod("ridl_resource_n")

#' @rdname ridl_resource_n
#' @export
ridl_resource_n.default <- function(dataset) {
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

#' Browse a RIDL object
#'
#' Browse a RIDL object
#'
#' @param x an RIDL object
#' @param ... Extra parameters
#' @rdname ridl_browse
#'
#'
#' @return Character Tags of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  ridl_config_set()
#'  res <- ridl_dataset_search(rows = 3L)
#'  ridl_browse(res[[1]])
#' }
ridl_browse <- function(x, ...)
  UseMethod("ridl_browse", x)

#' @rdname ridl_browse
#' @export
ridl_browse.default <- function(x, ...)
  x$browse()

#' Copy metadata from a RIDL object
#'
#' Copy metadata from a RIDL object
#'
#' @param x an RIDL object
#' @param configuration RIDLConfig, the configuration
#' @rdname ridl_clone
#'
#' @return Character Tags of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  # Setting the config to use RIDL default server
#'  ridl_config_set()
#'  res <- ridl_dataset_search(rows = 3L)
#'  ridl_clone(res[[1]])
#' }
ridl_clone <- function(x, configuration)
  UseMethod("ridl_clone")

#' @rdname ridl_clone
#' @export
ridl_clone.default <- function(x, configuration)
  stop("It only works with a RIDLObject",
       call. = TRUE)