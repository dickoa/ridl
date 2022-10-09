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
    ridl_resource_get_all = function(pattern = NULL, format = NULL) {
      l <- self$resources

      if (!is.null(pattern)) {
        b <- sapply(self$data$resources,
                    function(x)
                      grepl(pattern, x$name, ignore.case = TRUE))
        l <- l[b]
      }

      if (!is.null(format)) {
        b <- sapply(self$data$resources,
                    function(x)
                      tolower(x$format) %in% tolower(format))
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
    ridl_resource_count = function() {
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
        stop("This resource already have a dataset id",
             call. = FALSE)
      n <- length(self$data$resources)
      if (is.null(self$data$num_resources))
        self$data$num_resources <- n
      if (n > 0) {
        self$data$resources[[n + 1]] <- resource$data
        self$resources[[n + 1]] <- RIDLResource$new(resource$data)
        self$data$num_resources <- n + 1
      } else {
        self$data$resources[[1]] <- resource$data
        self$resources <- list(RIDLResource$new(resource$data))
        self$data$num_resources <- 1L
      }
    },

    #' @description
    #'
    #' Delete a dataset
    #'
    #' @param id character, the id or name of the dataset to delete
    #' @param configuration RIDLConfig, the configuration
    ridl_delete = function(id, configuration = NULL) {
      if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
        ridl_config_set(configuration = configuration)
      configuration <- ridl_config_get()
      res <- configuration$call_action("package_delete",
                                       list(id = id))
      res$raw$raise_for_status()
      res$result
    },

    #' @description
    #' Add a container to a dataset
    #' @param container_name the name of the container to add
    ridl_container_set = function(container_name) {
      self$data$owner_org <- container_name
    },

    #' @description
    #' Browse the dataset page on RIDL
    ridl_browse = function() {
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

    #' Get all collaborators for a given dataset
    #'
    #' Get all collaborators for a given dataset
    #'
    #' @param user character, id or name of the user
    #' @param capacity character, the user capacity
    #' @param configuration RIDLConfig, the configuration
    #' @return a character, the title
    collaborator_list = function(user = NULL,
                                 capacity = "member",
                                 configuration = NULL) {
      if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
        ridl_config_set(configuration = configuration)
      configuration <- ridl_config_get()
      res <- configuration$call_action("package_collaborator_list_for_user",
                                       list(id = user,
                                            capacity = capacity))
      res$raw$raise_for_status()
      res$result
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
      bool <- vapply(bool, isTRUE, logical(1))
      union("owner_org", nm[bool])
    },

    #' @description
    #' Check dataset required field
    #'
    #' @return a logical value, TRUE if the the dataset
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
      data_fields <- names(self$data)
      all_fields <- union(self$get_fields(), "resources")
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
      cat("  Resources (up to 5): ",
          sift_res(self$data$resources), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases RIDLDataset
as.list.RIDLDataset <- function(x, ...) {
  x$as_list()
}

#' @export
#' @aliases RIDLDataset
#' @importFrom tibble as_tibble
as_tibble.RIDLDataset <- function(x, ...) {
  data <- x$data
  num_resources <- data$num_resources
  fields <- x$get_fields()
  config <- x$get_config()
  url <- config$get_site_url()
  url <- paste0(url, "/dataset/", x$data$name)
  data <- drop_nulls(data[fields])
  data <- lapply(data, \(x)
                 if (is.list(x))
                   paste(unique(unlist(x)), collapse = ", ")
                 else
                   as.character(x))
  data$dataset_url <- url
  data$num_resources <- num_resources
  data <- as_tibble(data)
  data$dataset <- list(x)
  data
}

#' @export
#' @aliases RIDLDataset
as_tibble.ridl_dataset_list <- function(x) {
  l <- lapply(x, as_tibble)
  rbind_tibble(l)
}

#' Access the nth resource of a dataset
#'
#' Access the nth resource of a dataset
#'
#' @param dataset a RIDLDataset
#' @param n integer, resource position in the dataset
#'
#' @rdname ridl_dataset_resource_get
#'
#' @export
#' @return a RIDLResource
ridl_dataset_resource_get.RIDLDataset <- function(dataset, n) {
  dataset$ridl_resource_get(n)
}

#' @rdname ridl_dataset_resource_get
#' @export
rd_resource_get.RIDLDataset <- ridl_dataset_resource_get.RIDLDataset

#' List all the resources in a dataset
#'
#' List all the resources in a dataset
#'
#' @param dataset RIDLDataset
#' @param pattern character, regex pattern to match resource name
#' @param format character, format of the resources
#'
#' @rdname ridl_dataset_resource_get_all
#'
#' @export
#' @return A list of RIDLResource
ridl_dataset_resource_get_all.RIDLDataset <- function(dataset,
                                                      pattern = NULL,
                                                      format = NULL) {
  dataset$ridl_resource_get_all(pattern = pattern,
                                format = format)
}

#' @rdname ridl_dataset_resource_get_all
#' @export
rd_resource_get_all.RIDLDataset <- ridl_dataset_resource_get_all.RIDLDataset

#' Delete resource from a dataset
#'
#' Delete resource from a dataset
#'
#' @param dataset RIDLDataset the dataset from which we one to remove the resource
#' @param n integer, the index of the resource to be removed
#'
#' @rdname ridl_dataset_resource_delete
#'
#' @return Dataset the dataset without the resource
#' @export
ridl_dataset_resource_delete.RIDLDataset <- function(dataset, n) {
  dataset$ridl_resource_delete(n)
  dataset
}

#' @rdname ridl_dataset_resource_delete
#' @export
rd_resource_delete.RIDLDataset <- ridl_dataset_resource_delete.RIDLDataset

#' Delete all resources from dataset
#'
#' Delete all resources from dataset
#'
#' @param dataset A RIDLDataset, the dataset with resources remove
#'
#' @rdname ridl_dataset_resource_delete_all
#'
#' @return a RIDLDataset without resources
#' @export
ridl_dataset_resource_delete_all.RIDLDataset <- function(dataset) {
  dataset$ridl_resource_delete_all()
  dataset
}

#' @rdname ridl_dataset_resource_delete_all
#' @export
rd_resource_delete_all.RIDLDataset <- ridl_dataset_resource_delete_all.RIDLDataset

#' Search for datasets on RIDL
#'
#' Search for datasets on RIDL
#'
#' @rdname ridl_dataset_search
#'
#' @importFrom jsonlite fromJSON
#' @importFrom crul Paginator
#'
#' @param query character Query terms, use solr format
#' and default to "*:*" (match everything)
#' @param visibility character, either all, public or restricted
#' @param filter_query character, filter query results
#' @param rows integer, number of matching records to return. Defaults to 10.
#' @param start integer, the offset in the complete result for where
#' the set of returned datasets should begin.
#' @param page_size integer, Size of page to return. Defaults to 1000.
#' @param progress logical, progress bar. Default to FALSE
#' @param configuration RIDLConfig object.
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
#'  rd_search("sens chad", rows = 3L)
#' }
#'
#' @export
ridl_dataset_search <- function(query = "*:*",
                                visibility = c("all", "public", "restricted"),
                                filter_query = "",
                                rows = 10L,
                                start = 0L,
                                page_size = 1000L,
                                configuration = NULL,
                                progress = FALSE,
                                ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(progress = progress,
                    configuration = configuration)
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
  suppressMessages(cc$get(path = paste0("/api/3/action/",
                                        "package_search"),
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

#' @rdname ridl_dataset_search
#'
#' @export
rd_search <- ridl_dataset_search

#' Pull and show a RIDL dataset
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
  res$raw$raise_for_status()
  RIDLDataset$new(initial_data = res$result,
                  configuration = configuration)
}

#' @rdname ridl_dataset_show
#' @export
rd_show <- ridl_dataset_show

#' @rdname ridl_dataset_list
#' @export
ridl_dataset_list.default <- function(container = NULL,
                                      user = NULL,
                                      configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  if (!is.null(user)) {
    res <- configuration$call_action("package_collaborator_list_for_user",
                                     list(id = user,
                                          capacity = "member"))
    res$raw$raise_for_status()
    res <- unlist(res$result)
    res <- dataset_name_(res[names(res) %in% "package_id"])
  } else {
    res <- configuration$call_action("package_list")
    res$raw$raise_for_status()
    res <- unlist(res$result)
  }
  res
}

#' @rdname ridl_dataset_list
#' @export
rd_list.default <- ridl_dataset_list.default

#' Delete a dataset
#'
#' Delete a dataset
#'
#' @param dataset a RIDLDataset, the dataset to delete
#' @param configuration a RIDLConfig, the configuration
#'
#' @rdname ridl_dataset_delete
#'
#' @export
#' @return a RIDLResource
ridl_dataset_delete.RIDLDataset <- function(dataset, configuration = NULL) {
  dataset$ridl_delete(dataset$data$id,
                      configuration = configuration)
}

#' @rdname ridl_dataset_delete
#' @export
rd_delete.RIDLDataset <- ridl_dataset_delete.RIDLDataset

#' @rdname ridl_browse
#' @export
ridl_browse.RIDLDataset <- function(x, ...)
  x$ridl_browse()

#' Get the dataset container
#'
#' Get the container where the data is share
#'
#' @param dataset RIDLDataset, the dataset
#'
#' @rdname ridl_dataset_container_get
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
ridl_dataset_container_get.RIDLDataset <- function(dataset) {
  dataset$ridl_container_get()
}

#' @rdname ridl_dataset_container_get
#' @export
rd_container_get.RIDLDataset <- ridl_dataset_container_get.RIDLDataset

#' Add a dataset to a container
#'
#' Add a dataset to a container
#'
#' @param dataset RIDLDataset, the dataset
#' @param container_name charater, A valid RIDL container name, you can use
#' \code{ridl_container_list()} to have the list of all containers
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @rdname ridl_dataset_container_set
#'
#' @return A RIDLDataset
#' @export
#'
#' @examples
#' \dontrun{
#'  ds <- RIDLdataset$new(list(name = "cool-dataset"))
#'  rd_container_set(ds, "zimbabwe-shelter-nfi")
#' }
ridl_dataset_container_set.RIDLDataset <- function(dataset, container_name, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  assert_container_name(container_name)
  dataset$ridl_container_set(container_name)
  dataset
}

#' @rdname ridl_dataset_container_set
#' @export
rd_container_set.RIDLDataset <- ridl_dataset_container_set.RIDLDataset

#' Get the number of resources in the dataset
#'
#' Get the number of resources in the dataset
#'
#' @param dataset RIDLDataset
#'
#'
#' @return integer, the number of resources
#' @export
#'
#' @rdname ridl_dataset_resource_count
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL
#'  res <- ridl_dataset_search(rows = 3L, visibility = "public")
#'  rd_resource_count(res[[2]])
#' }
#' @export
ridl_dataset_resource_count.RIDLDataset <- function(dataset) {
  dataset$ridl_resource_count()
}

#' @rdname ridl_dataset_resource_count
#' @export
rd_resource_count.RIDLDataset <- ridl_dataset_resource_count.RIDLDataset

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
#' @rdname ridl_dataset_resource_add
#'
#' @return A RIDLDataset
#' @export
ridl_dataset_resource_add.RIDLDataset <- function(dataset, resource, ignore_dataset_id = FALSE, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  assert_resource(resource)
  dataset$ridl_resource_add(resource,
                            ignore_dataset_id = ignore_dataset_id)
  dataset
}

#' @rdname ridl_dataset_resource_add
#' @export
rd_resource_add.RIDLDataset <- ridl_dataset_resource_add.RIDLDataset

#' Create a RIDL dataset from list
#'
#' Create a RIDL dataset from list with required fields
#'
#' @param title character, Title(*) - Make sure to include: 'Survey name/title', 'Location', 'Country', and 'Year(s)' in the order indicated.
#' @param name character, URL(*) - The canonical name of the dataset, eg. my-dataset.
#' @param short_title character, Short title - eg. Short title for the project.
#' @param notes character, Description(*) - Some useful notes about the data. Please include the number of observations.
#' @param tag_string character,  Tags - eg. economy, mental health, government.
#' @param url character,  Project URL - Website URL associated with this data project (if applicable).
#' @param owner_org character, Data container(*)  - Use the canonical name for the container.
#' @param private character, Visibility (Private/Public).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param external_access_level character, External access level(*). Allowed values: `not_available` (Not available), `direct_access` (Direct access), `public_use` (Public use), `licensed_use` (Licensed use), `data_enclave` (Data enclave), `open_access` (Open access).
#' @param data_sensitivity character,  Data sensitivity - Apply to both Anonymized and Personally identifiable data. Allowed values: `yes` (Yes), `no` (No).
#' @param original_id character, Original ID - If the dataset already has an ID from the source org, DDI, etc...
#' @param data_collector character,  Data Collector(*) - Which organization owns / collected the data. Multiple values are allowed.
#' @param date_range_start Date, Date collection first date - Use dd/mm/yyyy format.
#' @param date_range_end Date, Date collection last date - Use dd/mm/yyyy format.
#' @param keywords character,  Topic classifications(*) - Tags useful for searching for the datasets. Multiple values are allowed.
#' @param unit_of_measurement character, Unit of measurement(*) - Unit of measurement / observation for the dataset.
#' @param sampling_procedure character, Sampling Procedure. Multiple values are allowed. Allowed values: `total_universe_complete_enumeration` (Total universe/Complete enumeration), `probability_simple_random` (Probability: Simple random), `probability_systematic_random` (Probability: Systematic random), `probability_stratified` (Probability: Stratified), `probability_stratified_proportional` (Probability: Stratified: Proportional), `probability_stratified_disproportional` (Probability: Stratified: Disproportional), `probability_cluster` (Probability: Cluster), `probability_cluster_simple_random` (Probability: Cluster: Simple random ), `probability_cluster_stratified_random` (Probability: Cluster: Stratified random), `probability_multistage` (Probability: Multistage), `nonprobability` (Non-probability), `nonprobability_availability` (Non-probability: Availability), `nonprobability_purposive` (Non-probability: Purposive), `nonprobability_quota` (Non-probability: Quota), `nonprobability_respondentassisted` (Non-probability: Respondent-assisted), `mixed_probability_nonprobability` (Mixed probability and non-probability), `other_other` (Use if the sampling procedure is known, but not found in the list..).
#' @param operational_purpose_of_data character,  Operational purpose of data - Classification of the type of data contained in the file. Multiple values are allowed. Allowed values: `participatory_assessments` (Participatory assessments), `baseline_household_survey` (Baseline Household Survey), `rapid_needs_assessment` (Rapid Needs Assessment), `protection_monitoring` (Protection Monitoring), `programme_monitoring` (Programme monitoring), `population_data` (Population Data), `cartography` (Cartography, Infrastructure & GIS).
#' @param process_status character, Dataset Process Status. Allowed values: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability. Allowd values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3rd level: Public Use File (PUF)).
#' @param geog_coverage character, Geographic Coverage - eg. National coverage, or name of the area, etc.
#' @param data_collection_technique character, Data collection technique(*). Allowed values: `nf` (Not specified), `f2f` (Face-to-face interview), `capi` (Face-to-face interview: Computerised), `cami` (Face-to-face interview: Mobile), `papi` (Face-to-face interview: Paper-and-pencil), `tri` (Telephone interview), `eri` (E-mail interview), `wri` (Web-based interview: audio-visual technology enabling the interviewer(s) and interviewee(s) to communicate in real time), `easi` (Self-administered questionnaire: E-mail), `pasi` (Self-administered questionnaire: Paper), `sasi` (Self-administered questionnaire: SMS/MMS), `casi` (Self-administered questionnaire: Computer-assisted), `cawi` (Self-administered questionnaire: Web-based), `foc` (Face-to-face focus group), `tfoc` (Telephone focus group), `obs` (Observation), `oth` (Other).
#' @param linked_datasets character, Linked Datasets - Links to other RIDL datasets. It supports multiple selections.
#' @param hxlated logical, Dataset with resources having HXL tags
#' @param archived logical, Archived(*) - Allows users to indicate if the dataset is archived or active. Allowed values: `False` (No), `True` (Yes).
#' @param admin_notes character, Admin Notes - General. You can use Markdown formatting here.
#' @param sampling_procedure_notes character, Admin Notes - Sampling Procedure. You can use Markdown formatting here.
#' @param response_rate_notes character, Admin Notes - Response Rate. You can use Markdown formatting here.
#' @param data_collection_notes character, Admin Notes - Data Collection. You can use Markdown formatting here.
#' @param weight_notes character, Admin Notes - Weighting. You can use Markdown formatting here.
#' @param clean_ops_notes character, Admin Notes - Cleaning. You can use Markdown formatting here.
#' @param data_accs_notes character, Admin Notes - Access authority. You can use Markdown formatting here.
#' @param ddi DDI.
#' @param kobo_asset_id character, the KoBoToolbox asset id.
#' @param configuration RIDLConfig, RIDL configuration used
#'
#' @return RIDLDataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  res <- ridl_dataset(name = "hum-dataset",
#'                      title = "Humanitarian dataset",
#'                      owner_org = "africa")
#'  res
#' }
ridl_dataset <- function(title,
                         name,
                         owner_org,
                         notes,
                         keywords,
                         visibility,
                         archived,
                         external_access_level,
                         unit_of_measurement,
                         data_collector,
                         data_collection_technique,
                         date_range_start = NULL,
                         date_range_end = NULL,
                         short_title = NULL,
                         tag_string  = NULL,
                         url = NULL,
                         private = NULL,
                         data_sensitivity = NULL,
                         original_id = NULL,
                         sampling_procedure = NULL,
                         operational_purpose_of_data = NULL,
                         hxlated = NULL,
                         process_status = NULL,
                         identifiability = NULL,
                         geog_coverage = NULL,
                         linked_datasets = NULL,
                         admin_notes = NULL,
                         sampling_procedure_notes = NULL,
                         response_rate_notes = NULL,
                         data_collection_notes = NULL,
                         weight_notes = NULL,
                         clean_ops_notes = NULL,
                         data_accs_notes = NULL,
                         ddi = NULL,
                         kobo_asset_id = NULL,
                         configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()

  data <-  list(title = title,
                name = name,
                short_title = short_title,
                notes = notes,
                tag_string = tag_string,
                url = url,
                owner_org = owner_org,
                private = private,
                visibility = visibility,
                external_access_level = external_access_level,
                data_sensitivity = data_sensitivity,
                original_id = original_id,
                data_collector = data_collector,
                date_range_start = date_range_start,
                date_range_end = date_range_end,
                keywords = keywords,
                unit_of_measurement = unit_of_measurement,
                sampling_procedure = sampling_procedure,
                operational_purpose_of_data = operational_purpose_of_data,
                `hxl-ated` = hxlated,
                process_status = process_status,
                identifiability = identifiability,
                geog_coverage = geog_coverage,
                data_collection_technique = data_collection_technique,
                linked_datasets = linked_datasets,
                archived = archived,
                admin_notes = admin_notes,
                sampling_procedure_notes = sampling_procedure_notes,
                response_rate_notes = response_rate_notes,
                data_collection_notes = data_collection_notes,
                weight_notes = weight_notes,
                clean_ops_notes = clean_ops_notes,
                data_accs_notes = data_accs_notes,
                ddi = ddi,
                kobo_asset_id = kobo_asset_id)

  data <- drop_nulls(data)
  data <- validate_dataset_data(data)
  RIDLDataset$new(data, configuration)
}

#' Create a dataset on RIDL
#'
#' Create a dataset on RIDL
#'
#' @param dataset RIDLDataset, the dataset to upload
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @export
ridl_dataset_create <-  function(dataset, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_dataset(dataset)
  data <- dataset$data

  if ("resources" %in% names(data)) {
    rs <- lapply(data$resources, function(r)
      RIDLResource$new(r, configuration))
    data$resources <- NULL

    res_ds <- configuration$call_action("package_create",
                                        body = data,
                                        verb = "post",
                                        encode = "json")
    res_ds$raw$raise_for_status()

    res_rs <- lapply(rs, function(r) {
      ridl_resource_create(resource = r,
                           dataset = RIDLDataset$new(res_ds$result),
                           configuration)
    })

    res <- list(dataset = res_ds,
                resources = res_rs)
  } else {
    res <- configuration$call_action("package_create",
                                     body = data,
                                     verb = "post",
                                     encode = "json")
    res$raw$raise_for_status()
  }
  invisible(res)
}

#' @rdname ridl_dataset_create
#' @export
rd_create <- ridl_dataset_create

#' Update a dataset on RIDL
#'
#' Update a dataset on RIDL
#'
#' @param dataset RIDLDataset, the dataset to be updated
#' @param title character, Title(*) - Make sure to include: 'Survey name/title', 'Location', 'Country', and 'Year(s)' in the order indicated.
#' @param name character, URL(*) - The canonical name of the dataset, eg. my-dataset.
#' @param short_title character, Short title - eg. Short title for the project.
#' @param notes character, Description(*) - Some useful notes about the data. Please include the number of observations.
#' @param tag_string character,  Tags - eg. economy, mental health, government.
#' @param url character,  Project URL - Website URL associated with this data project (if applicable).
#' @param owner_org character, Data container(*)  - Use the canonical name for the container.
#' @param private character, Visibility (Private/Public).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param external_access_level character, External access level(*). Allowed values: `not_available` (Not available), `direct_access` (Direct access), `public_use` (Public use), `licensed_use` (Licensed use), `data_enclave` (Data enclave), `open_access` (Open access).
#' @param data_sensitivity character,  Data sensitivity - Apply to both Anonymized and Personally identifiable data. Allowed values: `yes` (Yes), `no` (No).
#' @param original_id character, Original ID - If the dataset already has an ID from the source org, DDI, etc...
#' @param data_collector character,  Data Collector(*) - Which organization owns / collected the data. Multiple values are allowed.
#' @param date_range_start Date, Date collection first date - Use dd/mm/yyyy format.
#' @param date_range_end Date, Date collection last date - Use dd/mm/yyyy format.
#' @param keywords character,  Topic classifications(*) - Tags useful for searching for the datasets. Multiple values are allowed.
#' @param unit_of_measurement character, Unit of measurement(*) - Unit of measurement / observation for the dataset.
#' @param sampling_procedure character, Sampling Procedure. Multiple values are allowed. Allowed values: `total_universe_complete_enumeration` (Total universe/Complete enumeration), `probability_simple_random` (Probability: Simple random), `probability_systematic_random` (Probability: Systematic random), `probability_stratified` (Probability: Stratified), `probability_stratified_proportional` (Probability: Stratified: Proportional), `probability_stratified_disproportional` (Probability: Stratified: Disproportional), `probability_cluster` (Probability: Cluster), `probability_cluster_simple_random` (Probability: Cluster: Simple random ), `probability_cluster_stratified_random` (Probability: Cluster: Stratified random), `probability_multistage` (Probability: Multistage), `nonprobability` (Non-probability), `nonprobability_availability` (Non-probability: Availability), `nonprobability_purposive` (Non-probability: Purposive), `nonprobability_quota` (Non-probability: Quota), `nonprobability_respondentassisted` (Non-probability: Respondent-assisted), `mixed_probability_nonprobability` (Mixed probability and non-probability), `other_other` (Use if the sampling procedure is known, but not found in the list..).
#' @param operational_purpose_of_data character,  Operational purpose of data - Classification of the type of data contained in the file. Multiple values are allowed. Allowed values: `participatory_assessments` (Participatory assessments), `baseline_household_survey` (Baseline Household Survey), `rapid_needs_assessment` (Rapid Needs Assessment), `protection_monitoring` (Protection Monitoring), `programme_monitoring` (Programme monitoring), `population_data` (Population Data), `cartography` (Cartography, Infrastructure & GIS).
#' @param process_status character, Dataset Process Status. Allowed values: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability. Allowd values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3rd level: Public Use File (PUF)).
#' @param geog_coverage character, Geographic Coverage - eg. National coverage, or name of the area, etc.
#' @param data_collection_technique character, Data collection technique(*). Allowed values: `nf` (Not specified), `f2f` (Face-to-face interview), `capi` (Face-to-face interview: Computerised), `cami` (Face-to-face interview: Mobile), `papi` (Face-to-face interview: Paper-and-pencil), `tri` (Telephone interview), `eri` (E-mail interview), `wri` (Web-based interview: audio-visual technology enabling the interviewer(s) and interviewee(s) to communicate in real time), `easi` (Self-administered questionnaire: E-mail), `pasi` (Self-administered questionnaire: Paper), `sasi` (Self-administered questionnaire: SMS/MMS), `casi` (Self-administered questionnaire: Computer-assisted), `cawi` (Self-administered questionnaire: Web-based), `foc` (Face-to-face focus group), `tfoc` (Telephone focus group), `obs` (Observation), `oth` (Other).
#' @param linked_datasets character, Linked Datasets - Links to other RIDL datasets. It supports multiple selections.
#' @param hxlated logical, Dataset with resources having HXL tags
#' @param archived logical, Archived(*) - Allows users to indicate if the dataset is archived or active. Allowed values: `False` (No), `True` (Yes).
#' @param admin_notes character, Admin Notes - General. You can use Markdown formatting here.
#' @param sampling_procedure_notes character, Admin Notes - Sampling Procedure. You can use Markdown formatting here.
#' @param response_rate_notes character, Admin Notes - Response Rate. You can use Markdown formatting here.
#' @param data_collection_notes character, Admin Notes - Data Collection. You can use Markdown formatting here.
#' @param weight_notes character, Admin Notes - Weighting. You can use Markdown formatting here.
#' @param clean_ops_notes character, Admin Notes - Cleaning. You can use Markdown formatting here.
#' @param data_accs_notes character, Admin Notes - Access authority. You can use Markdown formatting here.
#' @param ddi DDI.
#' @param kobo_asset_id character, the KoBoToolbox asset id.
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @export
ridl_dataset_update <- function(dataset,
                                title,
                                name,
                                owner_org,
                                notes,
                                keywords,
                                visibility,
                                archived,
                                external_access_level,
                                unit_of_measurement,
                                data_collector,
                                data_collection_technique,
                                date_range_start = NULL,
                                date_range_end = NULL,
                                short_title = NULL,
                                tag_string  = NULL,
                                url = NULL,
                                private = NULL,
                                data_sensitivity = NULL,
                                original_id = NULL,
                                sampling_procedure = NULL,
                                operational_purpose_of_data = NULL,
                                hxlated = NULL,
                                process_status = NULL,
                                identifiability = NULL,
                                geog_coverage = NULL,
                                linked_datasets = NULL,
                                admin_notes = NULL,
                                sampling_procedure_notes = NULL,
                                response_rate_notes = NULL,
                                data_collection_notes = NULL,
                                weight_notes = NULL,
                                clean_ops_notes = NULL,
                                data_accs_notes = NULL,
                                ddi = NULL,
                                kobo_asset_id = NULL,
                                configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_dataset_on_ridl(dataset)

  data <-  list(title = title,
                name = name,
                short_title = short_title,
                notes = notes,
                tag_string = tag_string,
                url = url,
                owner_org = owner_org,
                private = private,
                visibility = visibility,
                external_access_level = external_access_level,
                data_sensitivity = data_sensitivity,
                original_id = original_id,
                data_collector = data_collector,
                date_range_start = date_range_start,
                date_range_end = date_range_end,
                keywords = keywords,
                unit_of_measurement = unit_of_measurement,
                sampling_procedure = sampling_procedure,
                operational_purpose_of_data = operational_purpose_of_data,
                `hxl-ated` = hxlated,
                process_status = process_status,
                identifiability = identifiability,
                geog_coverage = geog_coverage,
                data_collection_technique = data_collection_technique,
                linked_datasets = linked_datasets,
                archived = archived,
                admin_notes = admin_notes,
                sampling_procedure_notes = sampling_procedure_notes,
                response_rate_notes = response_rate_notes,
                data_collection_notes = data_collection_notes,
                weight_notes = weight_notes,
                clean_ops_notes = clean_ops_notes,
                data_accs_notes = data_accs_notes,
                ddi = ddi,
                kobo_asset_id = kobo_asset_id)

  data <- drop_nulls(data)
  data <- validate_dataset_data(data)

  res <- configuration$call_action("package_update",
                                   body = data,
                                   verb = "post",
                                   encode = "json")

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_dataset_update
#' @export
rd_update <- ridl_dataset_update

#' Patch a dataset on RIDL
#'
#' Patch a dataset on RIDL
#'
#' @param dataset RIDLDataset, the dataset to upload
#' @param title character, Title(*) - Make sure to include: 'Survey name/title', 'Location', 'Country', and 'Year(s)' in the order indicated.
#' @param name character, URL(*) - The canonical name of the dataset, eg. my-dataset.
#' @param short_title character, Short title - eg. Short title for the project.
#' @param notes character, Description(*) - Some useful notes about the data. Please include the number of observations.
#' @param tag_string character,  Tags - eg. economy, mental health, government.
#' @param url character,  Project URL - Website URL associated with this data project (if applicable).
#' @param owner_org character, Data container(*)  - Use the canonical name for the container.
#' @param private character, Visibility (Private/Public).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param external_access_level character, External access level(*). Allowed values: `not_available` (Not available), `direct_access` (Direct access), `public_use` (Public use), `licensed_use` (Licensed use), `data_enclave` (Data enclave), `open_access` (Open access).
#' @param data_sensitivity character,  Data sensitivity - Apply to both Anonymized and Personally identifiable data. Allowed values: `yes` (Yes), `no` (No).
#' @param original_id character, Original ID - If the dataset already has an ID from the source org, DDI, etc...
#' @param data_collector character,  Data Collector(*) - Which organization owns / collected the data. Multiple values are allowed.
#' @param date_range_start Date, Date collection first date - Use dd/mm/yyyy format.
#' @param date_range_end Date, Date collection last date - Use dd/mm/yyyy format.
#' @param keywords character,  Topic classifications(*) - Tags useful for searching for the datasets. Multiple values are allowed.
#' @param unit_of_measurement character, Unit of measurement(*) - Unit of measurement / observation for the dataset.
#' @param sampling_procedure character, Sampling Procedure. Multiple values are allowed. Allowed values: `total_universe_complete_enumeration` (Total universe/Complete enumeration), `probability_simple_random` (Probability: Simple random), `probability_systematic_random` (Probability: Systematic random), `probability_stratified` (Probability: Stratified), `probability_stratified_proportional` (Probability: Stratified: Proportional), `probability_stratified_disproportional` (Probability: Stratified: Disproportional), `probability_cluster` (Probability: Cluster), `probability_cluster_simple_random` (Probability: Cluster: Simple random ), `probability_cluster_stratified_random` (Probability: Cluster: Stratified random), `probability_multistage` (Probability: Multistage), `nonprobability` (Non-probability), `nonprobability_availability` (Non-probability: Availability), `nonprobability_purposive` (Non-probability: Purposive), `nonprobability_quota` (Non-probability: Quota), `nonprobability_respondentassisted` (Non-probability: Respondent-assisted), `mixed_probability_nonprobability` (Mixed probability and non-probability), `other_other` (Use if the sampling procedure is known, but not found in the list..).
#' @param operational_purpose_of_data character,  Operational purpose of data - Classification of the type of data contained in the file. Multiple values are allowed. Allowed values: `participatory_assessments` (Participatory assessments), `baseline_household_survey` (Baseline Household Survey), `rapid_needs_assessment` (Rapid Needs Assessment), `protection_monitoring` (Protection Monitoring), `programme_monitoring` (Programme monitoring), `population_data` (Population Data), `cartography` (Cartography, Infrastructure & GIS).
#' @param process_status character, Dataset Process Status. Allowed values: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability. Allowd values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3rd level: Public Use File (PUF)).
#' @param geog_coverage character, Geographic Coverage - eg. National coverage, or name of the area, etc.
#' @param data_collection_technique character, Data collection technique(*). Allowed values: `nf` (Not specified), `f2f` (Face-to-face interview), `capi` (Face-to-face interview: Computerised), `cami` (Face-to-face interview: Mobile), `papi` (Face-to-face interview: Paper-and-pencil), `tri` (Telephone interview), `eri` (E-mail interview), `wri` (Web-based interview: audio-visual technology enabling the interviewer(s) and interviewee(s) to communicate in real time), `easi` (Self-administered questionnaire: E-mail), `pasi` (Self-administered questionnaire: Paper), `sasi` (Self-administered questionnaire: SMS/MMS), `casi` (Self-administered questionnaire: Computer-assisted), `cawi` (Self-administered questionnaire: Web-based), `foc` (Face-to-face focus group), `tfoc` (Telephone focus group), `obs` (Observation), `oth` (Other).
#' @param linked_datasets character, Linked Datasets - Links to other RIDL datasets. It supports multiple selections.
#' @param hxlated logical, Dataset with resources having HXL tags
#' @param archived logical, Archived(*) - Allows users to indicate if the dataset is archived or active. Allowed values: `False` (No), `True` (Yes).
#' @param admin_notes character, Admin Notes - General. You can use Markdown formatting here.
#' @param sampling_procedure_notes character, Admin Notes - Sampling Procedure. You can use Markdown formatting here.
#' @param response_rate_notes character, Admin Notes - Response Rate. You can use Markdown formatting here.
#' @param data_collection_notes character, Admin Notes - Data Collection. You can use Markdown formatting here.
#' @param weight_notes character, Admin Notes - Weighting. You can use Markdown formatting here.
#' @param clean_ops_notes character, Admin Notes - Cleaning. You can use Markdown formatting here.
#' @param data_accs_notes character, Admin Notes - Access authority. You can use Markdown formatting here.
#' @param ddi DDI.
#' @param kobo_asset_id character, the KoBoToolbox asset id.
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @export
ridl_dataset_patch <-  function(dataset,
                                title = NULL,
                                name = NULL,
                                owner_org = NULL,
                                notes = NULL,
                                keywords = NULL,
                                visibility = NULL,
                                archived = NULL,
                                external_access_level = NULL,
                                unit_of_measurement = NULL,
                                data_collector = NULL,
                                data_collection_technique = NULL,
                                date_range_start = NULL,
                                date_range_end = NULL,
                                short_title = NULL,
                                tag_string  = NULL,
                                url = NULL,
                                private = NULL,
                                data_sensitivity = NULL,
                                original_id = NULL,
                                sampling_procedure = NULL,
                                operational_purpose_of_data = NULL,
                                hxlated = NULL,
                                process_status = NULL,
                                identifiability = NULL,
                                geog_coverage = NULL,
                                linked_datasets = NULL,
                                admin_notes = NULL,
                                sampling_procedure_notes = NULL,
                                response_rate_notes = NULL,
                                data_collection_notes = NULL,
                                weight_notes = NULL,
                                clean_ops_notes = NULL,
                                data_accs_notes = NULL,
                                ddi = NULL,
                                kobo_asset_id = NULL,
                                configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_dataset_on_ridl(dataset)

  data <-  list(title = title,
                name = name,
                short_title = short_title,
                notes = notes,
                tag_string = tag_string,
                url = url,
                owner_org = owner_org,
                private = private,
                visibility = visibility,
                external_access_level = external_access_level,
                data_sensitivity = data_sensitivity,
                original_id = original_id,
                data_collector = data_collector,
                date_range_start = date_range_start,
                date_range_end = date_range_end,
                keywords = keywords,
                unit_of_measurement = unit_of_measurement,
                sampling_procedure = sampling_procedure,
                operational_purpose_of_data = operational_purpose_of_data,
                `hxl-ated` = hxlated,
                process_status = process_status,
                identifiability = identifiability,
                geog_coverage = geog_coverage,
                data_collection_technique = data_collection_technique,
                linked_datasets = linked_datasets,
                archived = archived,
                admin_notes = admin_notes,
                sampling_procedure_notes = sampling_procedure_notes,
                response_rate_notes = response_rate_notes,
                data_collection_notes = data_collection_notes,
                weight_notes = weight_notes,
                clean_ops_notes = clean_ops_notes,
                data_accs_notes = data_accs_notes,
                ddi = ddi,
                kobo_asset_id = kobo_asset_id,
                id = dataset$data$id)
  data <- drop_nulls(data)

  res <- configuration$call_action("package_patch",
                                   body = data,
                                   verb = "post",
                                   encode = "json")

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_dataset_patch
#' @export
rd_patch <- ridl_dataset_patch

#' @rdname ridl_clone
#' @export
ridl_clone.RIDLDataset <- function(x, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  dataset <- x
  configuration <- ridl_config_get()
  assert_dataset(dataset)
  data <- dataset$data

  nm <- vapply(.ridl_dataset_schema$dataset_fields,
               function(x) x$field_name, character(1))

  data <- data[nm]

  RIDLDataset$new(data,
                  configuration = configuration)
}

#' Check if a dataset name or id is available on RIDL
#'
#' Check if a dataset name or id is available on RIDL
#'
#' @param dataset_name character, the name or id of the RIDLDataset
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @return A logical value, TRUE if the RIDLDataset exists
#' @export
ridl_dataset_exist <- function(dataset_name, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  res <- tryCatch({ridl_dataset_show(dataset_name);TRUE},
                  error = function(e) {
                    FALSE
                  })
  res
}

#' @rdname ridl_dataset_exist
#' @export
rd_exist <- ridl_dataset_exist
