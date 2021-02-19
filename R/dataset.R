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
        private$configuration <- get_ridl_config()
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
    #' @param index integer, the index of the resource to access
    #'
    #' @return a Resource object, the selected resource
    get_nth_resource = function(index) {
      n_res <- self$data$num_resources
      if (index > n_res)
        stop("Just ", n_res, "resource(s) available!",
             call. = FALSE)
      self$resources[[index]]
    },

    #' @description
    #' Get all resources of the dataset
    #'
    #' @param pattern character, regex pattern in resource name
    #' @param format character, format of the resources
    #'
    #' @return a list of Resource objects, all resources available in the dataset
    list_resources = function(pattern = NULL, format = NULL) {
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

      class(l) <- "ridl_resources_list"
      l
    },

    #' @description
    #' Get number of dataset resources
    #'
    #' @return The number of Resource objects
    number_of_resources = function() {
      length(self$resources)
    },

    #' @description
    #' Delete a resource by its index
    #'
    #' @param index, the index of the resource to delete
    delete_nth_resource = function(index = 1L) {
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
    delete_resources = function() {
      self$resources <- NULL
      self$data$resources <- NULL
      self$data$num_resources <- 0
    },

    #' @description
    #'
    #' @param resource RIDLResource, the resource
    #' @param ignore_dataset_id logical, ignore the dataset id
    #'
    #' Add a resource to a dataset
    add_resource = function(resource, ignore_dataset_id = FALSE) {
      if (!inherits(resource, "RIDLResource"))
        stop("Not of class `Resource` please use `RIDLResource$new()` to create a resource first!", call. = FALSE)
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
    add_container = function(container_name) {
      self$data$owner_org <- container_name
    },

    #' @description
    #' Browse the dataset page on RIDL
    browse = function() {
      url <- private$configuration$get_ridl_url()
      browseURL(url = paste0(url, "/dataset/", self$data$name))
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    get_configuration = function() {
      private$configuration
    },

    #' @description
    #' Get the dataset date
    #'
    #' @return a date, the dataset date.
    get_date_range = function() {
      date_start <- self$data$date_range_start
      date_end <- self$data$date_range_end
      if (is.null(date))
        date <- ""
      date
    },

    #' @description
    #' Get the dataset organization
    #'
    #' @return an Container object, the container where the data is
    get_container = function() {
      id <- self$data$organization$id
      pull_container(id)
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
        cat("  Container: ", self$data$organization$title, "\n", sep = "")
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


#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param index integer; resource position in the dataset
#' @export
#' @return Resource
get_dataset_nth_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$get_nth_resource(index)
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param pattern character, regex pattern in resource name
#' @param format character, format of the resources
#'
#' @export
#' @return resource_list
list_dataset_resources <- function(dataset, pattern = NULL,
                          format = NULL) {
  assert_dataset(dataset)
  dataset$list_resources(pattern = pattern,
                         format = format)
}

#' Delete resource from dataset
#'
#' Delete resource from dataset
#'
#' @details Delete resource from dataset
#'
#' @param dataset Dataset the dataset from which we one to remove the resource
#' @param index integer the index of the resource to be removed
#'
#' @return Dataset the dataset without the resource
#' @export
delete_dataset_nth_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_nth_resource(index)
  dataset
}

#' Delete resource from dataset
#'
#' Delete resource from dataset
#'
#' @details Delete resource from dataset
#'
#' @param dataset Dataset the dataset from which we one to remove the resource
#' @param index integer the index of the resource to be removed
#'
#' @return Dataset the dataset without the resource
#' @export
delete_dataset_nth_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_nth_resource(index)
  dataset
}

#' Delete all resource from dataset
#'
#' Delete all resource from dataset
#'
#' @param dataset A Dataset, the dataset to remove
#'
#' @details Delete all resources from dataset
#'
#' @return Dataset without resources
#' @export
delete_dataset_resources <- function(dataset) {
  assert_dataset(dataset)
  dataset$delete_resources()
  dataset
}

#' need to solve the issue with start and paginator
#' @importFrom jsonlite fromJSON
#' @rdname search_datasets
#' @noRd
.search_datasets  <-  function(query = "*:*",
                               visibility = c("all", "public", "restricted"),
                               filter_query = "",
                               rows = 10L,
                               start = 0L,
                               page_size = 1000L,
                               configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  visibility <- match.arg(visibility)
  if (visibility == "public")
    filter_query <- paste(filter_query,
                          paste0("visibility:", visibility),
                          sep = " AND ")
  if (visibility == "restricted")
    filter_query <- paste(filter_query,
                          paste0("visibility:", visibility),
                          sep = " AND ")
  cc <- crul::Paginator$new(client = configuration$remoteclient(),
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
  class(list_of_ds) <- "ridl_datasets_list"
  list_of_ds
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
#' @rdname search_datasets
#' @importFrom memoise memoise
#' @export
search_datasets <- memoise(.search_datasets)


#' @noRd
.pull_dataset <-  function(identifier, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("package_show",
                                   list(id = identifier))
  RIDLDataset$new(initial_data = res,
                  configuration = configuration)
}

#' Pull RIDL dataset into R
#'
#' Read a RIDL dataset from its name or id
#'
#' @param identifier Character dataset keyword
#' @param configuration a Configuration object
#'
#' @rdname pull_dataset
#' @return Dataset the dataset
#'
#' @importFrom memoise memoise
#' @export
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  res <- pull_dataset("mali-3wop")
#'  res
#' }
pull_dataset <- memoise(.pull_dataset)


#' @noRd
.list_datasets  <-  function(limit = NULL, offset = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  data <- drop_nulls(list(offset = offset, limit = limit))
  res <- configuration$call_action("package_list", data)
  unlist(res)
}

#' List datasets
#'
#' @param limit  integer; limit
#' @param offset integer; offset
#' @param configuration a Configuration object
#'
#' @importFrom memoise memoise
#'
#' @rdname list_datasets
#' @return A vector of datasets names
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  list_datasets(limit = 10L)
#' }
#'
#' @export
list_datasets <- memoise(.list_datasets)

#' @rdname list_datasets
#' @importFrom memoise memoise
#' @export
list_dataset_names <- memoise(.list_datasets)

#' @rdname browse
#' @export
browse.RIDLDataset <- function(x, ...)
  x$browse()

#' Dataset organization name
#'
#' Get the organization sharing the data
#'
#' @param dataset Dataset
#'
#'
#' @return Character The name of the organization sharing the data
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  res <- search_dataset(rows = 3L)
#'  get_container_name(res[[1]])
#' }
get_dataset_container_name <- function(dataset) {
  assert_dataset(dataset)
  dataset$data$organization$name
}


#' Dataset container
#'
#' Get the container where the data is share
#'
#' @param dataset RIDLDataset, the dataset
#'
#' @return A \code{RIDLContainer}
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  res <- search_dataset(rows = 3L, visibility = "public")
#'  get_dataset_container(res[[1]])
#' }
get_dataset_container <- function(dataset) {
  assert_dataset(dataset)
  dataset$get_container()
}

#' Dataset resources format
#'
#' Gets format of all resources from the datasets
#'
#' @param dataset Dataset
#'
#'
#'
#' @return Character Format of the resources
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  res <- search_dataset(rows = 3L)
#'  get_resources_formats(res[[1]])
#' }
get_dataset_resources_formats <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$list_resources(),
         function(resource)
           resource$get_format(), character(1))
}

#' Dataset tags name
#'
#' Gets dataset tags name
#'
#' @param dataset Dataset
#'
#'
#' @return Character Tags of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL
#'  set_ridl_config()
#'  res <- search_dataset(rows = 3L)
#'  get_tags_names(res[[1]])
#' }
get_dataset_tags_names <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$data$tags,
         function(tag) tag$name,
         character(1))
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
#' @return A RIDLDataset
#' @export
add_dataset_resource <- function(dataset, resource, ignore_dataset_id = FALSE, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  assert_dataset(dataset)
  assert_resource(resource)
  dataset$add_resource(resource,
                       ignore_dataset_id = ignore_dataset_id)
  dataset
}


#' Add organization to dataset
#'
#' Add organization to dataset
#'
#' @param dataset RIDLDataset, the dataset
#' @param container_name charater, A valid RIDL container name, you can use
#' \code{list_containers()} to have the list of all containers
#' @param configuration  RIDLConfig, the RIDL configuration
#'
#' @return A RIDLDataset
#' @export
#'
#' @examples
#' \dontrun{
#'  ds <- create_dataset(list(name = "cool-dataset"))
#'  add_dataset_container(ds, "zimbabwe-shelter-nfi")
#' }
add_dataset_container <- function(dataset, container_name, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  assert_dataset(dataset)
  assert_container_name(container_name)
  dataset$add_container(container_name)
  dataset
}

#' Create a RIDL dataset from list
#'
#' Create a RIDL dataset from list with required fields
#'
#' @param initial_data List, list of data
#' @param configuration RIDLConfig, RIDL configuration used
#'
#' @return Dataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  dsdata <- list(name = "hum-dataset",
#'                 title = "Humanitarian dataset")
#'  res <- create_dataset(dsdata)
#'  res
#' }
create_dataset <- function(initial_data, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  assert_valid_dataset_data(initial_data)
  RIDLDataset$new(initial_data)
}

#' @noRd
.push_dataset <-  function(dataset, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  assert_dataset(dataset)
  data <- dataset$data
  res <- configuration$call_action("package_create",
                                   body = data,
                                   verb = "post")
  res
}
