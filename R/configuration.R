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
#'  ridl_config()
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
    #' @importFrom logger appender_file log_appender log_formatter formatter_sprintf
    #' @importFrom lifecycle deprecated is_present deprecate_warn
    #'
    #' @param site character, the RIDL instance, prod (production server), test (testing server)
    #' @param token character, the RIDL API token
    #' @param key character, `r lifecycle::badge("deprecated")` the RIDL API key, is no longer recommended use the API `token` instead
    #' @param user_agent character value, User agent
    #' @param log_file character, the log file
    #' @param ... curl options to pass to crul::HttpClient
    #'
    #' @return A new Configuration object.
    initialize = function(site = "prod",
                          token = NULL,
                          key = NULL,
                          user_agent = NULL,
                          log_file = NULL,
                          ...) {

      config <- check_config_params(site = site,
                                    key = key,
                                    token = token,
                                    user_agent = user_agent)

      hooks <- NULL
      if (!is.null(log_file)) {
        Sys.setenv("RIDL_LOG" = log_file)
        self$data$log_file <- log_file
        log_appender(appender_file(log_file),
                     namespace = "ridl")
        log_formatter(formatter_sprintf,
                      namespace = "ridl")
        hooks <- list(request = log_request,
                      response = log_response)
      }

      self$data$token <- config$token
      self$data$site_url <- config$site_url
      headers <- list(Authorization = config$token)

      if (is.null(user_agent))
        user_agent <- get_user_agent()

      self$data$remoteclient <- HttpClient$new(url = config$site_url,
                                               headers = headers,
                                               opts = list(http_version = 2L,
                                                           useragent = user_agent, ...),
                                               hooks = hooks)
    },

    #' @description
    #' Specify a RIDL API token
    #'
    #' @param token a character with token
    set_token = function(token) {
      self$data$token <- token
    },

    #' @description
    #' Specify a RIDL API token
    #'
    #' @return a character, the RIDL API token
    get_token = function() {
      self$data$token
    },

    #' @description
    #' Get the RIDL server URL in use
    #' @return the server URL
    get_site_url = function() {
      self$data$site_url
    },

    #' @description
    #' Get the log file used by RIDL
    #' @return the log file
    get_log_file = function() {
      self$data$log_file
    },

    #' @description
    #' Get the remoteclient currently used
    #'
    #' @param ... curl options to pass to crul::HttpClient
    #'
    #' @importFrom crul set_opts
    #' @return a crul::HttpClient
    remoteclient = function(...) {
      set_opts(...)
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
      list(raw = res, result = parse_response(res))
    },

    #' @description
    #' read and show Configuration object
    #' @return Configuration object
    read = function() {
      self
    },

    #' @description
    #' Setup Configuration object
    #' @param site character, the RIDL instance, prod (production server), test (testing server)
    #' @param token a character value, the API token
    #' @param key a character value, the API key
    #' @param configuration a character
    #' @param ... curl options to pass to crul::HttpClient
    setup = function(site = "prod", token = NULL, key = NULL, configuration = NULL, ...) {
      if (!is.null(configuration)) {
        if (!inherits(configuration, "RIDLConfig,"))
          stop("Not a 'RIDLConfig' object!", call. = FALSE)
        private$shared$configuration <- configuration
      } else {
        private$shared$configuration <- RIDLConfig$new(site = site,
                                                       key = key,
                                                       token = token,
                                                       configuration = configuration,
                                                       ...)
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
      cat(paste0("  RIDL site url: ",
                 self$get_site_url()), sep = "\n")
      cat(paste0("  RIDL API token: ",
                 hide_token(self$get_token())), sep = "\n")
      invisible(self)
    }
  )
)

#' Create a RIDL configuration object
#'
#' Create and RIDL configuration object
#'
#' @param site character, the RIDL instance, prod (production server), test (testing server)
#' @param token character for the CKAN API token
#' @param key character for the CKAN API key
#' @param user_agent character value, a user agent string
#' @param log_file character, the log file
#' @param ... curl options to pass to crul::HttpClient
#'
#' @rdname ridl_config
#'
#' @return A RIDLConfig object
#' @export
#'
ridl_config <- function(site = "prod",
                        token = NULL,
                        key = NULL,
                        user_agent = NULL,
                        log_file = NULL,
                        ...) {
  RIDLConfig$new(site = site,
                 key = key,
                 token = token,
                 log_file = log_file,
                 user_agent = user_agent,
                 ...)
}

#' Set your RIDL configuration
#'
#' Sets the configuration settings for using RIDL.
#'
#' @param site character, the RIDL instance, prod (production server), test (testing server)
#' @param token character, the CKAN API token
#' @param key character, the CKAN API key
#' @param user_agent a character, A user agent string
#' @param log_file character, the log file
#' @param configuration RIDLConfig, the configuration
#' @param ... curl options to pass to crul::HttpClient
#'
#' @rdname ridl_config
#'
#' @details Setting up a configuration will help you access from a RIDL server
#'
#' @return Invisibly returns the ridl config object
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#' ridl_config_set(ridl_key = "xxxxxxxxxx")
#'
#' # You can check your configuration using \code{ridl_config_get}
#' config <- ridl_config_get()
#' config
#' }
ridl_config_set <- function(site = "prod",
                            token = NULL,
                            key = NULL,
                            user_agent = NULL,
                            log_file = NULL,
                            configuration = NULL,
                            ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig")) {
    .ridl_env$configuration <- configuration
  } else {
    .ridl_env$configuration <- RIDLConfig$new(site = site,
                                              key = key,
                                              token = token,
                                              log_file = log_file,
                                              user_agent = user_agent,
                                              ...)
  }
  ridl_memoise_clear()
}


#' @rdname ridl_config
#'
#' @export
ridl_config_setup <- ridl_config_set

#' @rdname ridl_config
#' @export
ridl_config_get <- function() {
  configuration <- .ridl_env$configuration
  assert_configuration(configuration)
  configuration$read()
}

#' Delete the ridl config
#'
#' Delete the configuration settings for using RIDL
#'
#'
#' @details Delete RIDL config
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use RIDL default server
#' ridl_config_set()
#' ridl_config_get()
#'
#' ridl_config_delete()
#' ridl_config_get()
#' }
ridl_config_delete <- function() {
  configuration <- ridl_config_get()
  configuration$delete()
  ridl_memoise_clear()
}
