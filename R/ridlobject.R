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
