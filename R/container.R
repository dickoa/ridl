#' RIDL Container
#'
#' RIDLContainer, it contains all the logic for creating, checking,
#' and updating resources
RIDLContainer <- R6::R6Class(
  classname = "RIDLContainer",
  inherit = RIDLObject,

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field data placeholder for the Container fields element
    data = NULL,

    #' @description
    #' Create a Container object
    #'
    #' @param initial_data list with required field to create a dataset
    #' @param configuration a Configuration object
    #' @return A Container object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "RIDLConfig")) {
        private$configuration <- ridl_config_get()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
    },

    #' @description
    #' Get the list of datasets within the container
    #' @return list of RIDLDataset objects
    list_datasets = function() {
      if (!"packages" %in% names(self$data))
        stop("No datasets available in this RIDLContainer!",
             call. = FALSE)
      vapply(self$data$packages, function(x) x$name, character(1))
    },

    #' @description
    #' Browse the Container page on RIDL
    ridl_browse = function() {
      url <- private$configuration$get_site_url()
      browseURL(url = paste0(url, "/organization/", self$data$name))
    },

    #' @description
    #' Get container fields
    #'
    #' @return list of fields for a dataset
    get_fields = function() {
      vapply(.ridl_container_schema$fields,
                   function(x) x$field_name, character(1))
    },

    #' @description
    #' Get container required fields
    #'
    #' @return list of required fields for a container
    get_required_fields = function() {
      nm <- self$get_fields()
      bool <- lapply(.ridl_container_schema$fields,
                     function(x) x$required)
      bool <- vapply(bool, function(x) !is.null(x), logical(1))
      nm[bool]
    },

    #' @description
    #' Check container required field
    #'
    #' @return a logical value, TRUE if the the container
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
      data_fields <- names(self$data)
      all_fields <- self$get_fields()
      required_fields <- self$get_required_fields()
      extra_fields <- setdiff(data_fields, all_fields)
      missing_required_fields <- setdiff(required_fields, data_fields)
      if (length(extra_fields) > 0)
        stop(sprintf("Field %s is not recognized or used to create a `RIDLContainer`\n",
                     extra_fields),
             call. = FALSE)
      if (length(missing_required_fields) > 0)
        stop(sprintf("Field %s is missing from the RIDLContainer object!\n",
                     missing_required_fields),
             call. = FALSE)
      invisible(self)
    },

    #' @description
    #' Get dataset field into list
    #'
    #' @return a list with container field element
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Dataset object
    print = function() {
      cat(paste0("<RIDL Container> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display name: ", self$data$display_name, "\n", sep = "")
      cat("  No. Datasets: ", self$data$package_count, "\n", sep = "")
      cat("  No. Members: ", length(self$data$users), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases RIDLContainer
#' @importFrom tibble as_tibble
as_tibble.RIDLContainer <- function(x, ...) {
  df <- tibble::tibble(container_id = x$data$id,
                       container_name = x$data$name)
  df$container <- list(x)
  df
}

#' @export
#' @aliases RIDLContainer
as.list.RIDLContainer <- function(x, ...) {
  x$as_list()
}

#' Pull a RIDL container
#'
#' Pull a RIDL container
#'
#' @param identifier character, the container name or id
#' @param configuration RIDLConfig, a configuration object
#' @param include_datasets logical, include datasets if TRUE
#' @param ... extra parameters for `organization_show` CKAN API endpoint
#'
#' @rdname ridl_container_show
#'
#' @return A RIDLContainer
#' @export
ridl_container_show <- function(identifier = NULL,
                                  include_datasets = TRUE,
                                  configuration = NULL,
                                ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  res <- configuration$call_action("organization_show",
                                   list(id = identifier,
                                        type = "data-container",
                                        include_datasets = include_datasets,
                                        ...))
  res$raw$raise_for_status()
  RIDLContainer$new(initial_data = res$result,
                    configuration = configuration)
}

#' @rdname ridl_browse
#' @export
ridl_browse.RIDLContainer <- function(x, ...)
  x$browse()

#' List RIDL containers
#'
#' List RIDL containers
#'
#' @param sort character how to sort the results. Default is "name asc"
#' @param user_container logical, list container for user, default to FALSE
#' @param configuration RIDLConfig, a configuration
#'
#' @rdname ridl_container_list
#'
#' @return A list of containers on RIDL
#' @export
ridl_container_list.default <- function(sort = c("title asc", "name",
                                                 "package_count", "title"),
                                        user_container = FALSE,
                                        configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()

  if (!user_container) {
    sort <- match.arg(sort)
    data <- drop_nulls(list(sort = sort,
                            all_fields = FALSE,
                            include_dataset_count = FALSE,
                            type = "data-container"))

    res <- configuration$call_action("organization_list",
                                     data)
    res$raw$raise_for_status()
    res <- unlist(res$result)
  } else {
    data <- drop_nulls(list(permission = "read",
                            include_dataset_count = FALSE,
                            type = "data-container"))
    res <- configuration$call_action("organization_list_for_user",
                                     data)
    res$raw$raise_for_status()
    res <- vapply(res$result, function(r) r$name,
                  character(1))
  }
  res
}

#' List RIDL datasets
#'
#' List RIDL datasets
#'
#' @param container RIDLContainer, the container containing the datasets
#' @param configuration a RIDLConfig, the configuration object
#'
#' @rdname ridl_dataset_list
#'
#' @return A vector of datasets names
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  ridl_config_set()
#'  list_ridl_dataset()
#' }
#'
#' @export
ridl_dataset_list.RIDLContainer <- function(container = NULL,
                                            configuration = NULL) {
  container$list_datasets()
}

#' Create a RIDL container from list
#'
#' Create a RIDL container from list with required fields
#'
#' @param data List, list of data
#' @param configuration RIDLConfig, RIDL configuration used
#'
#' @return RIDLContainer the container
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  dsdata <- list(name = "hum-container",
#'                 title = "Humanitarian container")
#'  res <- ridl_container(dsdata)
#'  res
#' }
ridl_container <- function(data, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  data <- validate_container_data(data)
  RIDLContainer$new(data, configuration)
}

#' Create a container on RIDL
#'
#' Create a container on RIDL
#'
#' @param container RIDLContainer, the container
#' @param configuration RIDLConfig, the configuration
#'
#' @return A HttpResponse object
#'
#' @export
ridl_container_create <-  function(container, configuration = NULL) {

  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container(container)
  data <- container$data
  res <- configuration$call_action("organization_create",
                                   body = data,
                                   verb = "post")
  invisible(res)
}

#' Update a container on RIDL
#'
#' Update a container on RIDL
#'
#' @param container RIDLContainer, the container
#' @param configuration RIDLConfig, the configuration
#'
#' @return A HttpResponse object
#'
#' @export
ridl_container_update <-  function(container, configuration = NULL) {

  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container(container)
  data <- container$data
  res <- configuration$call_action("organization_update",
                                   body = data,
                                   verb = "post")
  invisible(res)
}

#' Patch a container on RIDL
#'
#' Patch a container on RIDL
#'
#' @param container RIDLContainer, the container
#' @param configuration RIDLConfig, the configuration
#'
#' @return A HttpResponse object
#'
#' @export
ridl_container_patch <-  function(container, configuration = NULL) {

  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container(container)
  data <- container$data
  res <- configuration$call_action("organization_patch",
                                   body = data,
                                   verb = "post")
  invisible(res)
}

#' Copy a container metadata
#'
#' Copy a container metadata
#'
#' @param container RIDLContainer, the container to copy
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @return a RIDLContainer
#'
#' @export
ridl_container_copy_metadata <-  function(container, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container(container)
  data <- container$data

  nm <- vapply(.ridl_container_schema$fields,
               function(x) x$field_name, character(1))

  data <- data[nm]

  ridl_container(data,
                 configuration = configuration)
}

#' Check if a container name or id is available on RIDL
#'
#' Check if a container name or id is available on RIDL
#'
#' @param container_name character, the name or id of the RIDLContainer
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @return A logical value, TRUE if the RIDLContainer exists
#' @export
ridl_container_exist <- function(container_name, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

    res <- tryCatch({ridl_container_show(container_name);TRUE},
                    error = function(e) {
                      FALSE
                  })
    res
}
