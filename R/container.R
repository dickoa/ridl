#' RIDL Container
#'
#' RIDL Container
#'
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
    #' @return list of Dataset objects
    get_datasets = function() {
      if (!"packages" %in% names(self$data))
        stop("No datasets available, use Container$pull with `include_datasets = TRUE` and try again!",
             call. = FALSE)
      list_of_ds <- lapply(self$data$packages,
                           function(x) Dataset$new(initial_data = x))
      class(list_of_ds) <- "datasets_list"
      list_of_ds
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
#' @aliases Container
#' @importFrom tibble as_tibble
as_tibble.RIDLContainer <- function(x, ...) {
  df <- tibble::tibble(container_id = x$data$id,
                       container_name = x$data$name)
  df$container <- list(x)
  df
}

#' @export
#' @aliases Container
as.list.RIDLContainer <- function(x, ...) {
  x$as_list()
}

#' @noRd
.pull_container  <-  function(identifier = NULL,
                                 include_datasets = FALSE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("organization_show",
                                   list(id = identifier,
                                        type = "data-container",
                                        include_datasets = include_datasets, ...))
  RIDLContainer$new(initial_data = res, configuration = configuration)
}

#' Read a RIDL container
#'
#' Read a RIDL container
#'
#' @param identifier character resource uuid
#' @param configuration a RIDL configuration object
#' @param include_datasets Logical, include datasets if TRUE
#' @param ... Extra parameters
#' @rdname pull_container
#'
#' @return RIDL container
#' @export
pull_container <- memoise::memoise(.pull_container)

#' @rdname browse
#' @export
browse.RIDLContainer <- function(x, ...)
  x$browse()

#' @noRd
.list_containers  <-  function(sort = "name asc",
                               all_fields = FALSE,
                               include_dataset_count = TRUE,
                               include_groups = FALSE,
                               include_user = FALSE,
                               include_tags = FALSE,
                               configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  data <- drop_nulls(list(sort = sort,
                          all_fields = all_fields,
                          include_dataset_count = include_dataset_count,
                          include_groups = include_groups,
                          include_user = include_user,
                          include_tags = include_tags,
                          type = "data-container"))
  res <- configuration$call_action("organization_list", data)
  if (isFALSE(all_fields))
    res <- unlist(res)
  res
}


#' List RIDL container
#'
#' List RIDL container
#'
#' @param sort Character how to sort the results. Default is "name asc"
#' @param all_fields Logical, include all fields
#' @param include_dataset_count Logical include count in the result
#' @param include_groups Logical, whether or not to include locations
#' @param include_user Logical, whether or not to include user
#' @param include_tags Logical whether or not to include tags
#' @param configuration Configuration
#' @param ... extra paramaters
#'
#' @importFrom memoise memoise
#'
#' @rdname list_containers
#' @return A list of containers on RIDL
#' @export
list_containers <- memoise::memoise(.list_containers)

#' @rdname list_containers
#' @export
list_container_names <- memoise::memoise(.list_containers)
