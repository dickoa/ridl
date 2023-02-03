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
    #' Browse the Container page on RIDL
    ridl_browse = function() {
      url <- private$configuration$get_site_url()
      browseURL(url = paste0(url, "/data-container/", self$data$name))
    },

    #' @description
    #' Get container fields
    #'
    #' @return list of fields for a dataset
    get_fields = function() {
      url <- private$configuration$get_site_url()
      fields <- .ridl_container_schema$fields
      if (grepl("uat", url))
        fields <- .ridl_container_uat_schema$fields
      vapply(fields,
             function(x) x$field_name, character(1))
    },

    #' @description
    #' Get container required fields
    #'
    #' @return list of required fields for a container
    get_required_fields = function() {
      url <- private$configuration$get_site_url()
      fields <- .ridl_container_schema$fields
      if (grepl("uat", url))
        fields <- .ridl_container_uat_schema$fields
      nm <- self$get_fields()
      bool <- lapply(fields,
                     function(x) x$required)
      bool <- vapply(bool, isTRUE, logical(1))
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
      cat("  Display name: ", self$data$title, "\n", sep = "")
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
  data <- x$data
  num_datasets <- data$package_count
  config <- x$get_config()
  url <- config$get_site_url()
  url <- paste0(url, "/data-container/", x$data$name)
  fields <- vapply(.ridl_container_schema$fields,
                   \(x) x$field_name, character(1))
  data <- drop_nulls(data[fields])
  data <- lapply(data, \(x)
                 if (is.list(x))
                   paste(unique(unlist(x)), collapse = ", ")
                 else
                   as.character(x))
  data$num_datasets <- num_datasets
  data$container_url <- url
  data <- as_tibble(data)
  data$container <- list(x)
  data
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

#' @rdname ridl_container_show
#' @export
rc_show <- ridl_container_show

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

#' @rdname ridl_container_list
#' @export
rc_list.default <- ridl_container_list.default

#' Create a RIDL container from list
#'
#' Create a RIDL container from list with required fields
#'
#' @param title character, the title of the container
#' @param name character, the name of the container
#' @param description character, the description of the data
#' @param not_used character, name of the parent container
#' @param country character, a vector of UNHCR country code
#' @param geographic_area character, Geographic area referenced.
#' @param sectoral_area character, The sectoral areas covered by the datasets included within. Multiple values are allowed.
#' @param population character, Description of the population covered by the container
#' @param tag_string character, the tag eg. economy, mental health, government
#' @param visible_external logical, Visible to Partner users
#' @param configuration RIDLConfig, RIDL configuration used
#'
#' @return RIDLContainer the container
#' @export
#'
#' @examples
#' \dontrun{
#'  res <- ridl_container(country = "MLI",
#'                        visible_external = FALSE,
#'                        geographic_area = "Mali")
#'  res
#' }
ridl_container <- function(country,
                           visible_external,
                           geographic_area,
                           title = NULL,
                           name = NULL,
                           description = NULL,
                           not_used = NULL,
                           sectoral_area = NULL,
                           population = NULL,
                           tag_string = NULL,
                           configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()

  data <- list(country = country,
               visible_external = as_pylog(visible_external),
               geographic_area = geographic_area,
               title = title,
               name = name,
               description = description,
               not_used = not_used,
               sectoral_area = sectoral_area,
               population = population,
               tag_string = tag_string)

  data <- validate_container_data(data)
  RIDLContainer$new(data,
                    configuration)
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

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_container_create
#' @export
rc_create <- ridl_container_create

#' Update a container on RIDL
#'
#' Update a container on RIDL
#'
#' @param container RIDLContainer, the container to update
#' @param title character, the title of the container
#' @param name character, the name of the container
#' @param description character, the description of the data
#' @param not_used character, name of the parent container
#' @param country character, a vector of UNHCR country code
#' @param geographic_area character, Geographic area referenced.
#' @param sectoral_area character, The sectoral areas covered by the datasets included within. Multiple values are allowed.
#' @param population character, Description of the population covered by the container
#' @param tag_string character, the tag eg. economy, mental health, government
#' @param visible_external logical, Visible to Partner users
#' @param configuration RIDLConfig, the configuration
#'
#' @return A HttpResponse object
#'
#' @export
ridl_container_update <-  function(container,
                                   country,
                                   visible_external,
                                   geographic_area,
                                   title = NULL,
                                   name = NULL,
                                   description = NULL,
                                   not_used = NULL,
                                   sectoral_area = NULL,
                                   population = NULL,
                                   tag_string = NULL,
                                   configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container_on_ridl(container)

  data <- list(country = country,
               visible_external = as_pylog(visible_external),
               geographic_area = geographic_area,
               title = title,
               name = name,
               description = description,
               not_used = not_used,
               sectoral_area = sectoral_area,
               population = population,
               tag_string = tag_string,
               id = container$data$id)

  data <- drop_nulls(data)
  data <- validate_container_data(data)

  res <- configuration$call_action("organization_update",
                                   body = data,
                                   verb = "post")

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_container_update
#' @export
rc_update <- ridl_container_update

#' Patch a container on RIDL
#'
#' Patch a container on RIDL
#'
#' @param container RIDLContainer, the container to patch
#' @param title character, the title of the container
#' @param name character, the name of the container
#' @param description character, the description of the data
#' @param not_used character, name of the parent container
#' @param country character, a vector of UNHCR country code
#' @param geographic_area character, Geographic area referenced.
#' @param sectoral_area character, The sectoral areas covered by the datasets included within. Multiple values are allowed.
#' @param population character, Description of the population covered by the container
#' @param tag_string character, the tag eg. economy, mental health, government
#' @param visible_external logical, Visible to Partner users
#' @param configuration RIDLConfig, the configuration
#'
#' @return A HttpResponse object
#'
#' @export
ridl_container_patch <-  function(container,
                                  country = NULL,
                                  visible_external = NULL,
                                  geographic_area = NULL,
                                  title = NULL,
                                  name = NULL,
                                  description = NULL,
                                  not_used = NULL,
                                  sectoral_area = NULL,
                                  population = NULL,
                                  tag_string = NULL,
                                  configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_container_on_ridl(container)

  data <- list(country = country,
               visible_external = as_pylog(visible_external),
               geographic_area = geographic_area,
               title = title,
               name = name,
               description = description,
               not_used = not_used,
               sectoral_area = sectoral_area,
               population = population,
               tag_string = tag_string,
               id = container$data$id)

  data <- drop_nulls(data)

  res <- configuration$call_action("organization_patch",
                                   body = data,
                                   verb = "post")
  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_container_patch
#' @export
rc_patch <- ridl_container_patch

#' @rdname ridl_clone
#' @export
ridl_clone.RIDLContainer <-  function(x, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  container <- x
  configuration <- ridl_config_get()
  assert_container(container)
  data <- container$data

  nm <- vapply(.ridl_container_schema$fields,
               function(x) x$field_name, character(1))

  data <- data[nm]

  RIDLContainer$new(data,
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

#' @rdname ridl_container_exist
#' @export
rc_exist <- ridl_container_exist

#' List container containers hierarchy
#'
#' List container containers hierarchy
#'
#' @rdname ridl_container_hierarchy_list
#'
#' @param container RIDLContainer, the container
#'
#' @export
ridl_container_hierarchy_list.default <- function(container) {
  ct <- rc_show(container)
  idx <- which(ct$data$name %in% container)
  if (idx == 0 || length(ct$data$groups[[idx]]) == 0)
    return (NULL)
  c(ct$data$groups[[idx]]$name,
    ridl_container_hierarchy_list(ct$data$groups[[idx]]$name))
}

#' @rdname ridl_container_hierarchy_list
#' @export
rc_hierarchy_list.default <- ridl_container_hierarchy_list.default

#' List container containers hierarchy
#'
#' List container containers hierarchy
#'
#' @rdname ridl_container_hierarchy_list
#'
#' @param container RIDLContainer, the container
#'
#' @export
ridl_container_hierarchy_list.RIDLContainer <- function(container) {
  lapply(ridl_container_hierarchy_list(container$data$name), rc_show)
}

#' @rdname ridl_container_hierarchy_list
#' @export
rc_hierarchy_list.RIDLContainer <- ridl_container_hierarchy_list.RIDLContainer
