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
        private$configuration <- get_ridl_config()
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
    browse = function() {
      url <- private$configuration$get_ridl_site_url()
      browseURL(url = paste0(url, "/organization/", self$data$name))
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

#' @noRd
.pull_container  <-  function(identifier = NULL,
                              include_datasets = TRUE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("organization_show",
                                   list(id = identifier,
                                        type = "data-container",
                                        include_datasets = include_datasets,
                                        ...))
  RIDLContainer$new(initial_data = res,
                    configuration = configuration)
}

#' Pull a RIDL container
#'
#' Pull a RIDL container
#'
#' @param identifier character resource uuid
#' @param configuration a RIDL configuration object
#' @param include_datasets Logical, include datasets if TRUE
#' @param ... Extra parameters
#' @rdname pull_container
#'
#' @return A RIDLContainer
#' @export
pull_container <- memoise::memoise(.pull_container)

#' @rdname browse
#' @export
browse.RIDLContainer <- function(x, ...)
  x$browse()

#' @noRd
.list_containers  <-  function(sort = c("title asc", "name",
                                        "package_count", "title"),
                                    configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  sort <- match.arg(sort)
  data <- drop_nulls(list(sort = sort,
                          all_fields = FALSE,
                          include_dataset_count = FALSE,
                          type = "data-container"))
  res <- configuration$call_action("organization_list",
                                   data)
  unlist(res)
}

#' List RIDL containers
#'
#' List RIDL containers
#'
#' @param sort character how to sort the results. Default is "name asc"
#' @param configuration RIDLConfig, a configuration
#'
#' @importFrom memoise memoise
#'
#' @rdname list_containers
#'
#' @return A list of containers on RIDL
#' @export
list_containers.default <- memoise::memoise(.list_containers)

#' List RIDL datasets
#'
#' @param container RIDLContainer, the container containing the datasets
#' @param configuration a RIDLConfig, the configuration object
#'
#' @importFrom memoise memoise
#'
#' @rdname list_datasets
#'
#' @return A vector of datasets names
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  list_datasets()
#' }
#'
#' @export
list_datasets.RIDLContainer <- function(container = NULL, configuration = NULL) {
  container$list_datasets()
}