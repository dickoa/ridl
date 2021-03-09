#' RIDL Dataset
#'
#' RIDLDataset class containing all logic for accessing,
#' creating, and updating datasets and associated resources.
RIDLDataset <- R6::R6Class(
  classname = "RIDLDataset",
  inherit = RIDLObject,

  private = list(
    configuration = NULL
  ),
  public = list(
    #' @field resources list of Resource object within the dataset
    resources = NULL,
    #' @field data placeholder for Dataset field element
    data = list(),
    #' @description
    #' Create a new Dataset object
    #'
    #' @param initial_data list with required field to create a dataset
    #' @param configuration a Configuration object
    #' @return A Dataset object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "RIDLConfig")) {
        private$configuration <- ridl_config_get()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      key <- names(initial_data)
      self$data <- initial_data
      if ("resources" %in% key)
        self$resources <- lapply(self$data$resources,
                                 function(x)
                                   RIDLResource$new(initial_data = x,
                                                    configuration = configuration))
    },

    #' @description
    #' Get the nth resource of the dataset
    #'
    #' @param n integer, the index of the resource to access
    #'
    #' @return a Resource object, the selected resource
    ridl_resource_get = function(n) {
      n_res <- self$data$num_resources
      if (n > n_res)
        stop("Just ", n_res, "resource(s) available!",
             call. = FALSE)
      self$resources[[n]]
    },

    #' @description
    #' Get all resources of the dataset
    #'
    #' @param pattern character, regex pattern in resource name
    #' @param format character, format of the resources
    #'
    #' @return a list of Resource objects, all resources available in the dataset
    ridl_resource_list = function(pattern = NULL, format = NULL) {
      l <- self$resources

      if (!is.null(pattern)) {
        b <- sapply(self$data$resources,
                    function(x)
                      grepl(pattern, x$name, ignore.case = TRUE))
        l <- l[b]
      }

      if (!is.null(format)) {
        b <- sapply(self$data$resources,
                    function(x) tolower(x$format) %in% tolower(format))
        l <- l[b]
      }

      if (is.null(l) | length(l) < 1)
        l <- list()

      class(l) <- "ridl_resource_list"
      l
    },

    #' @description
    #' Get number of dataset resources
    #'
    #' @return The number of RIDLResource objects
    ridl_resource_n = function() {
      length(self$resources)
    },

    #' @description
    #' Delete a resource by its index
    #'
    #' @param index, the index of the resource to delete
    ridl_resource_delete = function(index = 1L) {
      n_resources <- self$data$num_resources
      if (n_resources == 0)
        stop("No Resources to delete!", call. = FALSE)
      if (index > n_resources)
        stop("Just ", n_resources, " Resource(s) available!",
             call. = FALSE)
      self$data$resources[[index]] <- NULL
      self$resources[[index]] <- NULL
      self$data$num_resources <- max(0, self$data$num_resources - 1)
    },

    #' @description
    #' Delete all resources from a dataset
    ridl_resource_delete_all = function() {
      self$resources <- NULL
      self$data$resources <- NULL
      self$data$num_resources <- 0
    },

    #' @description
    #'
    #' Add a resource to a dataset
    #'
    #' @param resource RIDLResource, the resource
    #' @param ignore_dataset_id logical, ignore the dataset id
    ridl_resource_add = function(resource, ignore_dataset_id = FALSE) {
      if (!inherits(resource, "RIDLResource"))
        stop("Not of class `RIDLResource` please use `ridl_resource` to create a resource first!", call. = FALSE)
      if ("package_id" %in% names(resource$data))
        stop("This Resource already have a dataset id", call. = FALSE)
      if (length(self$data$resources) > 0) {
        i <- self$data$num_resources
        self$data$resources[[i + 1]] <- resource$data
        self$resources[[i + 1]] <- RIDLResource$new(resource$data)
        self$data$num_resources <- self$data$num_resources + 1
      } else {
        self$data$resources[[1]] <- resource$data
        self$resources <- list(RIDLResource$new(resource$data))
        self$data$num_resources <- 1L
      }
    },

    #' @description
    #' Add a container to a dataset
    #' @param container_name the name of the container to add
    ridl_container_set = function(container_name) {
      self$data$owner_org <- container_name
    },

    #' @description
    #' Browse the dataset page on RIDL
    browse = function() {
      url <- private$configuration$get_site_url()
      browseURL(url = paste0(url, "/dataset/", self$data$name))
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    ridl_config_get = function() {
      private$configuration
    },

    #' @description
    #' Get the dataset date
    #'
    #' @return a date, the dataset date.
    get_date_range = function() {
      date_start <- self$data$date_range_start
      date_end <- self$data$date_range_end
      if (is.null(date_start))
        date_start <- ""
      if (is.null(date_end))
        date_end <- ""
      c(date_start, date_end)
    },

    #' @description
    #' Get the dataset container name
    #'
    #' @return a RIDLContainer, the container where the dataset is shared
    ridl_container_get = function() {
      id <- self$data$owner_org
      ridl_container_show(id)
    },

    #' @description
    #' Get the dataset container title
    #'
    #' Get the title of the dataset for pretty printing
    #'
    #' @return a character, the title
    ridl_container_get_title = function() {
      if ("id" %in% names(self$data)) {
        res <- self$data$organization$title
      } else {
        res <-  self$ridl_container_get()$data$title
      }
      res
    },

    #' @description
    #' Get dataset fields
    #'
    #' @return list of fields for a dataset
    get_fields = function() {
      vapply(.ridl_dataset_schema$dataset_fields,
                   function(x) x$field_name, character(1))
    },

    #' @description
    #' Get dataset required fields
    #'
    #' @return list of required fields for a dataset
    get_required_fields = function() {
      nm <- self$get_fields()
      bool <- lapply(.ridl_dataset_schema$dataset_fields,
                     function(x) x$required)
      bool <- vapply(bool, function(x) !is.null(x), logical(1))
      union("owner_org", nm[bool])
    },

    #' @description
    #' Check dataset required field
    #'
    #' @return a logical value, TRUE if the the dataset
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
      data_fields <- names(self$data)
      all_fields <- self$get_fields()
      required_fields <- self$get_required_fields()
      extra_fields <- setdiff(data_fields, all_fields)
      missing_required_fields <- setdiff(required_fields, data_fields)
      if (length(extra_fields) > 0)
        stop(sprintf("Field %s is not recognized or used to create a `RIDLDataset`\n",
                     extra_fields),
             call. = FALSE)
      if (length(missing_required_fields) > 0)
        stop(sprintf("Field %s is missing from the RIDLDataset object!\n",
                     missing_required_fields),
             call. = FALSE)
      invisible(self)
    },

    #' @description
    #' Get dataset field into list
    #'
    #' @return a list with dataset field
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Dataset object
    print = function() {
        cat(paste0("<RIDL Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Visibility: ", self$data$visibility, "\n", sep = "")
        cat("  Container: ", self$ridl_container_get_title(), "\n", sep = "")
        cat("  Resources (up to 5): ",
            sift_res(self$data$resources), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases Dataset
as.list.RIDLDataset <- function(x, ...) {
  x$as_list()
}

#' @export
#' @aliases RIDLDataset
#' @importFrom tibble as_tibble
as_tibble.RIDLDataset <- function(x, ...) {
  tibble::tibble(dataset_title = tolower(x$data$title),
                 dataset_name = x$data$name,
                 container_name = x$data$owner_org,
                 n_resources = ridl_resource_n(x),
                 dataset = list(x))
}

#' @export
#' @aliases RIDLResource
as_tibble.ridl_dataset_list <- function(x) {
  l <- lapply(x, as_tibble)
  Reduce(rbind, l)
}

#' Access the nth resource of a dataset
#'
#' Access the nth resource of a dataset
#'
#' @param dataset a RIDLDataset
#' @param n integer, resource position in the dataset
#'
#' @rdname ridl_resource_get
#'
#' @export
#' @return a RIDLResource
ridl_resource_get.RIDLDataset <- function(dataset, n) {
  dataset$ridl_resource_get(n)
}

#' List all the resources in a dataset
#'
#' List all the resources in a dataset
#'
#' @param dataset RIDLDataset
#' @param pattern character, regex pattern to match resource name
#' @param format character, format of the resources
#'
#' @rdname ridl_resource_list
#'
#' @export
#' @return A ridl_resource_list
ridl_resource_list.RIDLDataset <- function(dataset, pattern = NULL,
                                           format = NULL) {
  dataset$ridl_resource_list(pattern = pattern,
                             format = format)
}

#' Delete resource from a dataset
#'
#' Delete resource from a dataset
#'
#' @param dataset RIDLDataset the dataset from which we one to remove the resource
#' @param n integer, the index of the resource to be removed
#'
#' @rdname ridl_resource_delete
#'
#' @return Dataset the dataset without the resource
#' @export
ridl_resource_delete.RIDLDataset <- function(dataset, n) {
  dataset$ridl_resource_delete(n)
  dataset
}

#' Delete all resources from dataset
#'
#' Delete all resources from dataset
#'
#' @param dataset A RIDLDataset, the dataset with resources remove
#'
#' @rdname ridl_resource_delete_all
#'
#' @return a RIDLDataset without resources
#' @export
ridl_resource_delete_all.RIDLDataset <- function(dataset) {
  dataset$ridl_resource_delete_all()
  dataset
}

#' Search for datasets on RIDL
#'
#' Search for datasets on RIDL
#'
#' @param query character Query terms, use solr format
#' and default to "*:*" (match everything)
#' @param visibility character, all, public or restricted
#' @param filter_query character Filter Query results
#' @param rows integer; Number of matching records to return. Defaults to 10.
#' @param start integer; the offset in the complete result for where
#' the set of returned datasets should begin.
#' @param page_size integer; Size of page to return. Defaults to 1000.
#' @param configuration Configuration object.
#' @param ... Extra parameters for `package_search` endpoints
#'
#' @details Search and find datasets on RIDL
#'
#'
#' @return A list of RIDL datasets
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL default server
#'  search_datasets("displaced nigeria", rows = 3L)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom crul Paginator
#'
#' @rdname ridl_dataset_search
#'
#' @export
ridl_dataset_search <- function(query = "*:*",
                                visibility = c("all", "public", "restricted"),
                                filter_query = "",
                                rows = 10L,
                                start = 0L,
                                page_size = 1000L,
                                configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  visibility <- match.arg(visibility)
  if (visibility == "public")
    filter_query <- paste(filter_query,
                          paste0("visibility:", visibility),
                          sep = " AND ")
  if (visibility == "restricted")
    filter_query <- paste(filter_query,
                          paste0("visibility:", visibility),
                          sep = " AND ")
  cc <- Paginator$new(client = configuration$remoteclient(),
                      by = "limit_offset",
                      limit_param = "rows",
                      offset_param = "start",
                      limit = rows,
                      chunk = page_size)
  suppressMessages(cc$get(path = paste0("/api/3/action/", "package_search"),
                          list(q = query, fq = filter_query, ...)))
  list_of_ds <- fromJSON(cc$parse(),
                         simplifyVector = FALSE)$result$results
  list_of_ds <- lapply(list_of_ds,
                       function(x)
                         RIDLDataset$new(initial_data = x,
                                         configuration = configuration))
  class(list_of_ds) <- "ridl_dataset_list"
  list_of_ds
}

#' Pull a RIDL dataset
#'
#' Read a RIDL dataset from its name or id
#'
#' @param identifier character, name or id of the dataset
#' @param configuration RIDLConfig, the configuration to use if any
#'
#' @rdname ridl_dataset_show
#' @return Dataset the dataset
#'
#' @export
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  res <- ridl_dataset_show("unhcr-mrt-2017-sea-1-1")
#'  res
#' }
ridl_dataset_show <-  function(identifier, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  res <- configuration$call_action("package_show",
                                   list(id = identifier))
  RIDLDataset$new(initial_data = res,
                  configuration = configuration)
}

#' @rdname ridl_dataset_list
#' @export
ridl_dataset_list.default <- function(container = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  res <- configuration$call_action("package_list")
  unlist(res)
}

#' @rdname browse
#' @export
browse.RIDLDataset <- function(x, ...)
  x$browse()

#' Get the dataset container
#'
#' Get the container where the data is share
#'
#' @param dataset RIDLDataset, the dataset
#'
#' @rdname ridl_container_get
#'
#' @return A \code{RIDLContainer}
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  res <- ridl_dataset_search(rows = 3L, visibility = "public")
#'  ridl_container_get(res[[1]])
#' }
ridl_container_get.RIDLDataset <- function(dataset) {
  dataset$ridl_container_get()
}

#' Get the number of resources in the dataset
#'
#' Get the number of resources in the dataset
#'
#' @param dataset RIDLDataset
#'
#' @rdname ridl_resource_n
#'
#' @return integer, the number of resources
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL
#'  res <- ridl_dataset_search(rows = 3L, visibility = "public")
#'  ridl_resource_n(res[[2]])
#' }
#' @export
ridl_resource_n.RIDLDataset <- function(dataset) {
  dataset$ridl_resource_n()
}

#' Add a Resource to a dataset
#'
#' Add a Resource to a dataset
#'
#'
#' @param dataset RIDLDataset, the dataset
#' @param resource RIDLResource, the resource
#' @param ignore_dataset_id logical, ignore the dataset id
#' @param configuration RIDLConfig, the configuration
#'
#' @rdname ridl_resource_add
#'
#' @return A RIDLDataset
#' @export
ridl_resource_add.RIDLDataset <- function(dataset, resource, ignore_dataset_id = FALSE, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  assert_resource(resource)
  dataset$ridl_resource_add(resource,
                            ignore_dataset_id = ignore_dataset_id)
  dataset
}

#' Add a dataset to a container
#'
#' Add a dataset to a container
#'
#' @param dataset RIDLDataset, the dataset
#' @param container_name charater, A valid RIDL container name, you can use
#' \code{ridl_container_list()} to have the list of all containers
#' @param configuration  RIDLConfig, the RIDL configuration
#'
#' @rdname ridl_container_set
#'
#' @return A RIDLDataset
#' @export
#'
#' @examples
#' \dontrun{
#'  ds <- ridl_dataset(list(name = "cool-dataset"))
#'  ridl_container_set(ds, "zimbabwe-shelter-nfi")
#' }
ridl_container_set.RIDLDataset <- function(dataset, container_name, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  assert_container_name(container_name)
  dataset$ridl_container_set(container_name)
  dataset
}

#' Create a RIDL dataset from list
#'
#' Create a RIDL dataset from list with required fields
#'
#' @param data List, list of data
#' @param configuration RIDLConfig, RIDL configuration used
#'
#' @return RIDLDataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  dsdata <- list(name = "hum-dataset",
#'                 title = "Humanitarian dataset")
#'  res <- ridl_dataset(dsdata)
#'  res
#' }
ridl_dataset <- function(data, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  assert_valid_dataset_data(data)
  RIDLDataset$new(data, configuration)
}

#' @noRd
ridl_dataset_create <-  function(dataset, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  assert_dataset(dataset)
  data <- dataset$data
  res <- configuration$call_action("package_create",
                                   body = data,
                                   verb = "post")
  res
}

#' @noRd
ridl_dataset_update <-  function(dataset, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  assert_dataset(dataset)
  data <- dataset$data
  res <- configuration$call_action("package_update",
                                   body = data,
                                   verb = "post")
  res
}
