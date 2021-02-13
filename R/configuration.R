#' RIDL Configuration
#'
#' RIDL Configuration allow to connect to a RIDL server
#' and setup project where you can interact with the RIDL platform
#'
#' @format NULL
#' @usage NULL
#'
#' @importFrom crul HttpClient
#'
#' @examples
#' \dontrun{
#' set_ridl_config()
#' get_rhd_config()
#' }
RIDLConfig <- R6::R6Class(
  classname = "RIDLConfig",
  private = list(shared = .ridl_env),
  public = list(
    #' @field data all info in list.
    data = list(),

    #' @description
    #' Create a new Configuration object.
    #'
    #' @importFrom crul HttpClient
    #'
    #' @param ridl_key character, the RIDL API key
    #' @param user_agent a character value, User agent
    #' @return A new Configuration object.
    initialize = function(ridl_key = NULL, user_agent = NULL) {
      check_config_params(ridl_key = ridl_key,
                          user_agent = user_agent)

      ridl_site <- "https://ridl.unhcr.org"

      if (is.null(ridl_key)) {
        ridl_key_env <- Sys.getenv("RIDL_API_KEY")
        if (ridl_key_env == "")
          stop("You need to properly set the `RIDL_API_KEY` variable or use the `ridl_key parameter.`",
               call. = FALSE)
          ridl_key <- ridl_key_env
      }

      Sys.setenv("RIDL_API_KEY" = ridl_key)
      self$data$ridl_key <- ridl_key
      headers <- list(`X-CKAN-API-Key` = ridl_key)

      if (is.null(user_agent))
        user_agent <- get_user_agent()

      self$data$remoteclient <- HttpClient$new(url = ridl_site,
                                               headers = headers,
                                               opts = list(http_version = 2L,
                                                           useragent = user_agent))
    },

    #' @description
    #' Configuration credentials when using a RIDL API key
    get_credentials = function() {
    },

    #' @description
    #' Specify a RIDL API key
    #'
    #' @param ridl_key a character with key
    set_ridl_key = function(ridl_key) {
      if (!is_valid_uuid(key))
        stop("key not valid!", call. = FALSE)
      self$data$ridl_key <- ridl_key
    },

    #' @description
    #' Specify a RIDL API key
    #'
    #' @return a character, the RIDL API key
    get_ridl_key = function() {
      self$data$ridl_key
    },

    #' @description
    #' Get the RIDL server URL in use
    #' @return the server URL
    get_ridl_url = function() {
      "https://ridl.unhcr.org"
    },

    #' @description
    #' Get the remoteclient currently used
    #' @return a crul::HttpClient
    remoteclient = function() {
      self$data$remoteclient
    },

    #' @description
    #' Call the client to the RIDL API
    #'
    #' @param action a character
    #' @param ... parameters for each verb used
    #' @param verb a character the verb used, `post`, `get`, `put` or `patch`
    #' @return list a with status code and results
    call_action = function(action, ..., verb = "get") {
      if (!verb %in% c("post", "get", "put", "patch"))
        stop("Only `get`, `post`, `put` and `patch` are supported!")
      cli <- self$data$remoteclient
      action_path <- paste0("/api/3/action/", action)
      res <- cli$verb(verb, path = action_path, ...)
      parse_response(res)
    },

    #' @description
    #' read and show Configuration object
    #' @return Configuration object
    read = function() {
      self
    },

    #' @description
    #' Setup Configuration object
    #'
    #' @param configuration a character
    #' @param ridl_key a character value, the API key
    setup = function(ridl_key = NULL, configuration = NULL) {
      if (!is.null(configuration)) {
        if (!inherits(configuration, "RIDLConfig,"))
          stop("Not a 'RIDLConfig' object!", call. = FALSE)
        private$shared$configuration <- configuration
      } else {
        private$shared$configuration <- RIDLConfig$new(ridl_key = ridl_key,
                                                          ridl_config = ridl_config)
      }
    },

    #' @description
    #' Delete a Configuration object
    delete = function() {
      private$shared$configuration <- NULL
    },


    #' @description
    #' Convert configuration to list
    #' @return configuration in list format
    as_list = function() {
      self$data
    },

    #' @description
    #' Print Configuration object
    print = function() {
      cat("<RIDL Configuration> ", sep = "\n")
      cat(paste0("  RIDL site: ", self$get_ridl_url()), sep = "\n")
      cat(paste0("  RIDL API key: ", self$get_ridl_key()), sep = "\n")
      invisible(self)
    }
  )
)

#' Create a RIDL configuration object
#'
#' Create and RIDL configuration object
#' @param ridl_key Character for the CKAN API key, it is required to push data into RIDL
#' @param user_agent a character value, A user agent
#' you can push metdata and data to RIDL
#' @return An RIDL Configuration object
#' @export
create_ridl_config <- function(ridl_key = NULL,
                               user_agent = NULL) {
  RIDLConfig$new(ridl_key = ridl_key,
                    user_agent = user_agent)
}

#' Set ridl config
#'
#' Sets the configuration settings for using ridl.
#'
#' @param ridl_key Character for the CKAN API key, it is required to push data into RIDL
#' @param user_agent a character value, A user agent
#' you can push metdata and data to RIDL
#' @param configuration Configuration object.
#'
#' @rdname set_ridl_config
#'
#' @details Setting up a configuration will help you access from a RIDL server
#'
#'
#' @return Invisibly returns the ridl config object
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#' set_ridl_config(ridl_site = "demo")
#'
#' # You can check your configuration using \code{get_ridl_config}
#' config <- get_ridl_config()
#' config
#' }
set_ridl_config <- function(ridl_key = NULL,
                            user_agent = NULL,
                            configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig")) {
    .ridl_env$configuration <- configuration
  } else {
    .ridl_env$configuration <- RIDLConfig$new(ridl_key = ridl_key,
                                                 user_agent = user_agent)
  }
}

#' @rdname set_ridl_config
#' @export
get_ridl_config <- function() {
  configuration <- .ridl_env$configuration
  assert_configuration(configuration)
  configuration$read()
}

#' Delete ridl config
#'
#' Delete the configuration settings for using ridl.
#'
#'
#' @details Delete RIDL config
#'
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#' set_ridl_config()
#' get_ridl_config()
#'
#' delete_ridl_config()
#' get_ridl_config()
#' }
delete_ridl_config <- function() {
  configuration <- get_ridl_config()
  configuration$delete()
}
