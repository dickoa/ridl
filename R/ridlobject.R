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
ridl_dataset_list <- function(container, user, configuration)
  UseMethod("ridl_dataset_list")

#' @rdname ridl_dataset_list
#' @export
rd_list <- function(container, user, configuration)
  UseMethod("rd_list")

#' @rdname ridl_dataset_delete
#' @export
ridl_dataset_delete <- function(dataset, configuration)
  UseMethod("ridl_dataset_delete")

#' @rdname ridl_dataset_delete
#' @export
rd_delete <- function(dataset, configuration)
  UseMethod("rd_delete")

#' @rdname ridl_container_list
#' @export
ridl_container_list <- function(sort, user_container, configuration)
  UseMethod("ridl_container_list")

#' @rdname ridl_container_list
#' @export
rc_list <- function(sort, user_container, configuration)
  UseMethod("rc_list")

#' @rdname ridl_container_hierarchy_list
#' @export
ridl_container_hierarchy_list <- function(container_name)
  UseMethod("ridl_container_hierarchy_list")

#' @rdname ridl_container_hierarchy_list
#' @export
rc_hierarchy_list <- function(container_name)
  UseMethod("rc_hierarchy_list")

#' @rdname ridl_resource_file_format
#' @export
ridl_resource_file_format <- function(resource)
  UseMethod("ridl_resource_file_format")

#' @rdname ridl_resource_file_format
#' @export
rr_file_format <- function(resource)
  UseMethod("rr_file_format")

#' @rdname ridl_resource_file_format
#' @export
ridl_resource_file_format.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_format
#' @export
rr_file_format.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_excel_sheets
#' @export
ridl_resource_excel_sheets <- function(resource, format, download_folder, quiet)
  UseMethod("ridl_resource_excel_sheets")

#' @rdname ridl_resource_excel_sheets
#' @export
ridl_resource_excel_sheets.default <- function(resource, format, download_folder, quiet) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_excel_sheets
#' @export
rr_excel_sheets <- function(resource, format, download_folder, quiet)
  UseMethod("rr_excel_sheets")

#' @rdname ridl_resource_excel_sheets
#' @export
rr_excel_sheets.default <- function(resource, format, download_folder, quiet) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload_set <- function(resource, file_to_upload)
  UseMethod("ridl_resource_file_to_upload_set")

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload_set.default <- function(resource, file_to_upload) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_set <- function(resource, file_to_upload)
  UseMethod("rr_file_to_upload_set")

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_set.default <- function(resource, file_to_upload) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload_get <- function(resource)
  UseMethod("ridl_resource_file_to_upload_get")

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload_get.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_get <- function(resource)
  UseMethod("rr_file_to_upload_get")

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_get.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload <- function(resource)
  UseMethod("ridl_resource_file_to_upload")

#' @rdname ridl_resource_file_to_upload
#' @export
ridl_resource_file_to_upload.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
`ridl_resource_file_to_upload<-` <- function(resource, value)
  UseMethod("ridl_resource_file_to_upload<-")

#' @rdname ridl_resource_file_to_upload
#' @export
`ridl_resource_file_to_upload<-.default` <- function(resource, value) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}


#' @rdname ridl_resource_file_to_upload
#' @export
`rr_file_to_upload<-` <- function(resource, value)
  UseMethod("rr_file_to_upload<-")

#' @rdname ridl_resource_file_to_upload
#' @export
`rr_file_to_upload<-.default` <- function(resource, value) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload <- function(resource)
  UseMethod("rr_file_to_upload")

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_dataset
#' @export
rr_dataset <- function(resource)
  UseMethod("rr_dataset")

#' @rdname ridl_resource_dataset
#' @export
rr_dataset.default <- function(resource) {
  stop("Only available for a RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_get
#' @export
ridl_dataset_resource_get <- function(dataset, n)
  UseMethod("ridl_dataset_resource_get")

#' @rdname ridl_dataset_resource_get
#' @export
ridl_dataset_resource_get.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_get
#' @export
rd_resource_get <- function(dataset, n)
  UseMethod("rd_resource_get")

#' @rdname ridl_dataset_resource_get
#' @export
rd_resource_get.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_get_all
#' @export
ridl_dataset_resource_get_all <- function(dataset, pattern, format)
  UseMethod("ridl_dataset_resource_get_all")

#' @rdname ridl_dataset_resource_get_all
#' @export
ridl_dataset_resource_get_all.default <- function(dataset, pattern, format) {
    stop("Only available for a RIDLDataset object",
         call. = TRUE)
}

#' @rdname ridl_dataset_resource_get_all
#' @export
rd_resource_get_all <- function(dataset, pattern, format)
  UseMethod("rd_resource_get_all")

#' @rdname ridl_dataset_resource_get_all
#' @export
rd_resource_get_all.default <- function(dataset, pattern, format) {
    stop("Only available for a RIDLDataset object",
         call. = TRUE)
}

#' @rdname ridl_dataset_resource_delete
#' @export
ridl_dataset_resource_delete <- function(dataset, n)
  UseMethod("ridl_dataset_resource_delete")

#' @rdname ridl_dataset_resource_delete
#' @export
ridl_dataset_resource_delete.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_delete
#' @export
rd_resource_delete <- function(dataset, n)
  UseMethod("rd_resource_delete")

#' @rdname ridl_dataset_resource_delete
#' @export
rd_resource_delete.default <- function(dataset, n) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_delete_all
#' @export
ridl_dataset_resource_delete_all <- function(dataset)
  UseMethod("ridl_dataset_resource_delete_all")

#' @rdname ridl_dataset_resource_delete_all
#' @export
ridl_dataset_resource_delete_all.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_delete_all
#' @export
rd_resource_delete_all <- function(dataset)
  UseMethod("rd_resource_delete_all")

#' @rdname ridl_dataset_resource_delete_all
#' @export
rd_resource_delete_all.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_add
#' @export
ridl_dataset_resource_add <- function(dataset, resource, ignore_dataset_id, configuration)
  UseMethod("ridl_dataset_resource_add")

#' @rdname ridl_dataset_resource_add
#' @export
ridl_dataset_resource_add.default <- function(dataset, resource, ignore_dataset_id, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_resource_add
#' @export
rd_resource_add <- function(dataset, resource, ignore_dataset_id, configuration)
  UseMethod("rd_resource_add")

#' @rdname ridl_dataset_resource_add
#' @export
rd_resource_add.default <- function(dataset, resource, ignore_dataset_id, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_delete
#' @export
ridl_dataset_delete <- function(dataset, configuration)
  UseMethod("ridl_dataset_delete")

#' @rdname ridl_dataset_delete
#' @export
ridl_dataset_delete.default <- function(dataset, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_delete
#' @export
rd_delete <- function(dataset, configuration)
  UseMethod("rd_delete")

#' @rdname ridl_dataset_delete
#' @export
rd_delete.default <- function(dataset, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_container_set
#' @export
ridl_dataset_container_set <- function(dataset, container_name, configuration)
  UseMethod("ridl_dataset_container_set")

#' @rdname ridl_dataset_container_set
#' @export
ridl_dataset_container_set.default <- function(dataset, container_name, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_container_set
#' @export
rd_container_set <- function(dataset, container_name, configuration)
  UseMethod("rd_container_set")

#' @rdname ridl_dataset_container_set
#' @export
rd_container_set.default <- function(dataset, container_name, configuration) {
  stop("It's only working with RIDLDataset",
       call. = FALSE)
}

#' @rdname ridl_dataset_container_get
#' @export
ridl_dataset_container_get <- function(dataset)
  UseMethod("ridl_dataset_container_get")

#' @rdname ridl_dataset_container_get
#' @export
ridl_dataset_container_get.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_container_get
#' @export
rd_container_get <- function(dataset)
  UseMethod("ridl_dataset_container_get")

#' @rdname ridl_dataset_container_get
#' @export
rd_container_get.default <- function(dataset) {
  stop("Only available for a RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_count
#' @export
ridl_dataset_resource_count <- function(dataset)
  UseMethod("ridl_dataset_resource_count")

#' @rdname ridl_dataset_resource_count
#' @export
ridl_dataset_resource_count.default <- function(dataset) {
  stop("It only works with RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_dataset_resource_count
#' @export
rd_resource_count <- function(dataset)
  UseMethod("rd_resource_count")

#' @rdname ridl_dataset_resource_count
#' @export
rd_resource_count.default <- function(dataset) {
  stop("It only works with RIDLDataset object",
       call. = TRUE)
}

#' @rdname ridl_resource_read
#' @export
ridl_resource_read <- function(resource, sheet, format,
                               download_folder, force_download,
                               quiet_download, ...)
  UseMethod("ridl_resource_read")

#' @rdname ridl_resource_read
#' @export
ridl_resource_read.default <- function(resource, sheet, format,
                                       download_folder, force_download,
                                       quiet_download, ...) {
  stop("It only works with RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_read
#' @export
rr_read <- function(resource, sheet, format,
                    download_folder, force_download,
                    quiet_download, ...)
  UseMethod("rr_read")

#' @rdname ridl_resource_read
#' @export
rr_read.default <- function(resource, sheet, format,
                            download_folder, force_download,
                            quiet_download, ...) {
  stop("It only works with RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_download
#' @export
ridl_resource_download <- function(resource, folder, filename, quiet, force, ...)
  UseMethod("ridl_resource_download")

#' @rdname ridl_resource_download
#' @export
ridl_resource_download.default <- function(resource, folder, filename, quiet, force, ...) {
  stop("It only works with RIDLResource object",
       call. = TRUE)
}

#' @rdname ridl_resource_download
#' @export
rr_download <- function(resource, folder, filename, quiet, force, ...)
  UseMethod("rr_download")

#' @rdname ridl_resource_download
#' @export
rr_download.default <- function(resource, folder, filename, quiet, force, ...) {
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
