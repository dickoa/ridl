#' RIDL Dataset
#'
#' Dataset class containing all logic for accessing,
#' creating, and updating datasets and associated resources.
#'
Dataset <- R6::R6Class(
  classname = "Dataset",
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
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
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
                                   Resource$new(initial_data = x,
                                                configuration = configuration))
    },

    #' @description
    #' Get a specific resource of the dataset
    #'
    #' @param index integer, the index of the resource to access
    #'
    #' @return a Resource object, the selected resource
    get_dataset_resource = function(index) {
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
    list_dataset_resources = function(pattern = NULL, format = NULL) {
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

      class(l) <- "resources_list"
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
    delete_dataset_resource = function(index = 1L) {
      n_resources <- self$data$num_resources
      if (n_resources == 0)
        stop("No resources to delete!", call. = FALSE)
      if (index > n_resources)
        stop("Just ", n_resources, " resource(s) available!",
             call. = FALSE)
      self$data$resources[[index]] <- NULL
      self$resources[[index]] <- NULL
      self$data$num_resources <- max(0, self$data$num_resources - 1)
    },

    #' @description
    #' Delete all resources from a dataset
    delete_all_dataset_resources = function() {
      self$resources <- NULL
      self$data$resources <- NULL
      self$data$num_resources <- 0
    },

    #' @description
    #' Browse the dataset page on RIDL
    browse = function() {
      url <- private$configuration$get_ridl_url()
      browseURL(url = paste0(url, "dataset/", self$data$name))
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
    get_dataset_date_range = function() {
      date_start <- self$data$date_range_start
      date_end <- self$data$date_range_end
      if (is.null(date))
        date <- ""
      date
    },

    #' @description
    #' Get the dataset maintainer
    #'
    #' @return An User object, the maintainer of the dataset
    get_maintainer = function() {
      id <- self$data$maintainer
      pull_user(id)
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
        cat("  Resources (up to 5): ",
            sift_res(self$data$resources), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases Dataset
as.list.Dataset <- function(x, ...) {
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
get_dataset_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$get_dataset_resource(index)
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
  dataset$list_dataset_resources(pattern = pattern,
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
delete_dataset_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_dataset_resource(index)
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
delete_all_dataset_resources <- function(dataset) {
  assert_dataset(dataset)
  dataset$delete_all_dataset_resources()
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
                               start = 0L, page_size = 1000L,
                               configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
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
                            by = "query_params",
                            limit_param = "rows",
                            offset_param = "start",
                            limit = rows,
                            limit_chunk = page_size)
  suppressMessages(cc$get(path = paste0("/api/3/action/", "package_search"),
                          list(q = query, fq = filter_query, ...)))
  list_of_ds <- fromJSON(cc$parse(),
                         simplifyVector = FALSE)$result$results
  list_of_ds <- lapply(list_of_ds,
                       function(x)
                         Dataset$new(initial_data = x,
                                     configuration = configuration))
  class(list_of_ds) <- "datasets_list"
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
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("package_show",
                                   list(id = identifier))
  Dataset$new(initial_data = res,
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


#' List datasets
#'
#' List datasets
#'
#' @param limit  integer; limit
#' @param offset integer; offset
#' @param configuration a Configuration object
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
.list_datasets  <-  function(limit = NULL, offset = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  data <- drop_nulls(list(offset = offset, limit = limit))
  res <- configuration$call_action("package_list", data)
  unlist(res)
}

#' @rdname list_datasets
#' @importFrom memoise memoise
#' @export
list_datasets <- memoise(.list_datasets)

#' @rdname browse
#' @export
browse.Dataset <- function(x, ...)
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
get_container_name <- function(dataset) {
  assert_dataset(dataset)
  dataset$data$organization$name
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
get_resources_formats <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_resources(),
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
get_tags_names <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$data$tags,
         function(tag) tag$name,
         character(1))
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param index integer, resource position in the dataset
#'
#' @export
#' @return Resource
get_dataset_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$get_dataset_resource(index)
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param pattern regex pattern in resource name
#' @param format format of the resources
#'
#' @export
#' @return resource_list
list_all_dataset_resources <- function(dataset,
                                      pattern = NULL,
                                      format = NULL) {
  assert_dataset(dataset)
  dataset$list_all_dataset_resources(pattern = pattern, format = format)
}

#' Delete resource from dataset
#'
#' Delete resource from dataset
#'
#' @details Delete resource from dataset
#'
#' @param dataset Dataset the dataset from which we one to remove the resource
#' @param index Integer the index of the resource to be removed
#'
#' @return Dataset the dataset without the resource
#' @export
delete_dataset_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_dataset_resource(index)
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
delete_all_dataset_resources <- function(dataset) {
  assert_dataset(dataset)
  dataset$delete_all_dataset_resources()
  dataset
}
