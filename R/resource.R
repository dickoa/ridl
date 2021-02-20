#' RIDL Resource
#'
#' RIDLResource, it contains all the logic for creating, checking,
#' and updating resources
RIDLResource <- R6::R6Class(
  classname = "RIDLResource",
  inherit = RIDLObject,

  private = list(
    configuration = NULL,
    download_folder_ = NULL
  ),

  public = list(
    #' @field data placeholder for Resource field element
    data = NULL,

    #' @description
    #' Create a new RIDLResource object
    #'
    #' @param initial_data list with required field to create a resource
    #' @param configuration a RIDLConfig object
    #'
    #' @return A RIDLResource object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "RIDLConfig")) {
        private$configuration <- get_ridl_config()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()

      class <- vapply(initial_data, class, character(1))
      form_file_index <- class == "form_file"

      if (any(form_file_index)) {
        initial_data <- c(drop_nulls(initial_data[!form_file_index]),
                          initial_data[form_file_index])
      } else {
        initial_data <- drop_nulls(initial_data)
      }
      self$data <- initial_data
    },

    #' @description
    #' Download a RIDL resource
    #'
    #' @importFrom tools file_path_as_absolute
    #'
    #' @param folder a character, folder to save the dataset
    #' @param filename a character, filename of the dataset
    #' @param quiet a logical value, silent download if TRUE
    #' @param force a logical value, force download
    #'
    #' @return a character, the file path
    download = function(folder = NULL, filename = NULL,
                        quiet = TRUE, force = FALSE) {


      if (is.null(folder)) {
        folder <- ridl_cache_get_dir()
        if (!dir.exists(folder))
          dir.create(folder, recursive = TRUE)
      }

      if (is.null(filename)) {
        filename <- gsub("\\?.*", "", self$data$url)
        filename <- basename(filename)
      }

      url <- self$data$url

      url <- gsub("https://ridl.unhcr.org",
                  "",
                  url)

      con <- get_ridl_config()
      cli <- con$remoteclient()

      file_path <- file.path(folder,
                             filename)

      if (!file.exists(file_path) | force)
        cli$get(path = url,
                disk = file_path)

      private$download_folder_ <- tools::file_path_as_absolute(folder)
      invisible(file_path_as_absolute(file_path))
    },

    #' @description
    #' Get the download folder for the latest downloaded resource
    #'
    #' @importFrom tools file_path_as_absolute
    #'
    #' @return a character, folder with the latest downloaded resource
    download_folder = function() {
      file_path_as_absolute(private$download_folder_)
    },

    #' @description
    #' Read a resource object directly into memory
    #'
    #'
    #' @param sheet a character value, only for resource in Excel format
    #' @param format character, specify file format in case the automatic reader doesn't work as expected
    #' @param download_folder a character value, folder to save the downloaded resource
    #' @param force_download a logical value, if TRUE force download
    #' @param quiet_download a logical value, if TRUE silent download
    #' @param ... other parameters

    #' @return A \code{tibble}
    read_resource = function(sheet = NULL, format = NULL,
                             download_folder = NULL,
                             force_download = FALSE,
                             quiet_download = TRUE, ...) {

      if (!is.null(private$download_folder_) & is.null(download_folder))
        folder <- self$download_folder()

      file_path <- self$download(folder = download_folder,
                                 quiet = quiet_download,
                                 force = force_download)

      if (is.null(format))
        format <- self$get_format()

      hxl <- any(grepl("hxl",
                       tag_names(self$get_dataset()),
                       ignore.case = TRUE))

      switch(format,
             csv = read_ridl_delim(file_path, hxl = hxl, ...),
             xlsx = read_ridl_excel(file_path, sheet = sheet, hxl = hxl, ...),
             xls = read_ridl_excel(file_path, sheet = sheet, hxl = hxl, ...),
             `.csv` = read_ridl_delim(file_path, hxl = hxl, ...),
             `.xlsx` = read_ridl_excel(file_path, sheet = sheet, hxl = hxl, ...),
             `.xls` = read_ridl_excel(file_path, sheet = sheet, hxl = hxl, ...),
             stata = read_ridl_stata(file_path, ...),
             dta = read_ridl_stata(file_path, ...),
             `.dta` = read_ridl_stata(file_path, ...))
    },

    #' @description
    #' Get the list of sheets name of resource
    #'
    #'
    #' @param download_folder a character value, folder to save the downloaded resource
    #' @param format character, specify file format in case the automatic reader doesn't work as expected
    #' @param force_download a logical value, if TRUE force download
    #' @param quiet_download a logical value, if TRUE silent download
    #'
    #' @return the names of the sheets of XLS(X) resources
    get_sheets = function(format = NULL, download_folder = NULL,
                          quiet_download = TRUE, force_download = FALSE) {

      if (!is.null(private$download_folder_) & is.null(download_folder))
        folder <- self$download_folder()

      file_path <- self$download(folder = download_folder,
                                 quiet = quiet,
                                 force = force_download)
y
      if (is.null(format))
      format <- self$get_format()

      if (!format %in% c("xlsx", "xls"))
        stop("`get_sheets work only with Excel file", call. = FALSE)

      switch(format,
             xlsx = get_ridl_sheets_(file_path),
             xls = get_ridl_sheets_(file_path))
    },

    #' @description
    #' Get the resource dataset.
    #'
    #' @return a RIDLDataset, the dataset containing the resource
    get_dataset = function() {
      dataset_id <- self$data$package_id
      if (is.null(dataset_id)) {
        stop("Resource has no dataset id!", call. = FALSE)
      } else {
        pull_dataset(dataset_id)
      }
    },

    #' @description
    #' Get dataset required fields
    #'
    #' @return list of required fields for a resource
    get_required_fields = function() {
      private$configuration$data$ridl_config$resource$required_fields
    },

    #' @description
    #' Check dataset required field
    #' @param check_dataset_id logical whether to check or not dataset id
    #' @return a logical value, TRUE if the the resource is not missing
    #' a required field and throws an error otherwise
    check_required_field = function(check_dataset_id = FALSE) {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (check_dataset_id)
        # remove package_id
        n1 <- setdiff(n1, "package_id")
      if (!all(n1 %in% n2))
        stop(sprintf("Field %s is missing in the Resource object!\n",
                     setdiff(n1, n2)), call. = FALSE)
    },

    #' @description
    #' Get the file format
    #' @return a character, the file format of the resource
    get_format = function() {
      tolower(self$data$format)
    },

    #' @description
    #' Get resource field into list
    #'
    #' @return a list with resource field
    as_list = function() {
      self$data
    },

    #' @description
    #' Browse the resource page on RIDL
    browse = function() {
      url <- private$configuration$get_ridl_site_url()
      dataset_id <- self$data$package_id
      resource_id <- self$data$id
      browseURL(url = paste0(url, "/dataset/",
                             dataset_id, "/resource/", resource_id))
    },

    #' @description
    #' Print a Resource object
    print = function() {
      cat(paste0("<RIDL Resource> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$description, "\n", sep = "")
      cat("  Type: ", self$data$file_type, "\n", sep = "")
      cat("  Size: ", self$data$size, "\n", sep = "")
      cat("  Format: ", self$data$format, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases RIDLResource
#' @importFrom tibble as_tibble
as_tibble.RIDLResource <- function(x, ...) {
  df <- tibble::tibble(
    resource_id = x$data$id,
    resource_name = x$data$name,
    resource_format = tolower(x$data$format),
    resource_url = x$data$url)
  df$resource <- list(x)
  df
}

#' @export
#' @aliases RIDLResource
as_tibble.ridl_resources_list <- function(x) {
  l <- lapply(x, as_tibble)
  Reduce(rbind, l)
}

#' @export
#' @aliases RIDLResource
as.list.RIDLResource <- function(x, ...) {
  x$as_list()
}

#' Download a RIDL resource
#'
#' Download a RIDL resource into a specific folder
#'
#' @param resource RIDLResource, a RIDL resource
#' @param folder character, path of the directory where you will store the data
#' @param filename character, name of the file you will download
#' @param quiet logical, no progress bar from download (default = \code{FALSE})
#' @param force logical, force download (default = \code{FALSE})
#' @param ... extra paramaters
#'
#' @return character, the download folder path
#' @export
#'
#' @examples
#' \dontrun{
#' #Setting the config to use RIDL default server
#'  res <- read_resource("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#'  download_resource(res, folder = "/tmp")
#' }
download_resource <- function(resource, folder = NULL,
                              filename = NULL, quiet = FALSE, force = FALSE, ...) {
  assert_resource(resource)
  resource$download(folder = folder, filename = filename,
                    quiet = quiet, force = force, ...)
}

#' Get the names of the sheets of XLS(X) resources
#'
#' Get the names of the sheets of XLS(X) resources
#'
#' @param resource Resource, a RIDL resource
#' @param format character, specify file format in case the automatic reader doesn't work as expected
#' @param download_folder character, path of the directory where you will store the data
#' @param quiet logical, no progress bar from download (default = FALSE)
#'
#' @rdname get_sheets
#'
#' @return the names of the sheets of XLS(X) resources
#'
#' @export
get_sheets.RIDLResource <- function(resource,
                                    format = NULL,
                                    download_folder = NULL,
                                    quiet = TRUE) {
  resource$get_sheets(format = format,
                      download_folder = download_folder,
                      quiet = quiet)
}

#' Get the file format of the resource
#'
#' Get the file format of the resource
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname get_format
#'
#' @return A character, the format of the resource
#'
#' @export
get_format.RIDLResource <- function(resource) {
  resource$get_format()
}

#' Get the dataset containing the resource
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname get_dataset
#'
#' @return a RIDLDataset, the dataset containing the resource
#'
#' @export
 get_dataset.RIDLResource <- function(resource) {
  resource$get_dataset()
}

#' Read a RIDL Resource
#'
#' Read a RIDL Resource
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @param sheet character, the name of the sheet to read if XLS(X) resources. The first sheet is read by default.
#' @param format character, file format, csv, zipped csv, excel, xlsx, zipped shapefile, etc.
#' @param download_folder character, the path of the folder to store the
#' downloaded data
#' @param force_download logical, force download if TRUE
#' @param quiet_download logical, silent download
#' @param ... extra parameters
#' @return A \code{tibble}
#'
#' @export
read_resource <- function(resource, sheet = NULL,
                          format = NULL, download_folder = NULL,
                          force_download = FALSE, quiet_download = TRUE, ...) {
  assert_resource(resource)
  resource$read_resource(sheet = sheet,
                         format = format,
                         download_folder = download_folder,
                         force_download = force_download,
                         quiet_download = quiet_download,
                         ...)
}

#' @rdname search_resources
#' @noRd
.search_resources  <-  function(query = "*:*", configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("resource_search", list(query = query, ...))
  list_of_rs <- lapply(res$results, function(x)
    RIDLResource$new(initial_data = x,
                 configuration = configuration))
  class(list_of_rs) <- "ridl_resources_list"
  list_of_rs
}

#' Search for resources
#'
#' Search for resources
#'
#' @importFrom memoise memoise
#'
#' @param query character, a query
#' @param configuration RIDLConfig, a configuration
#' @param ... extra params
#'
#' @rdname search_resources
#' @details Search and find datasets on RIDL
#'
#'
#' @return A list of RIDLDataset
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL default server
#'  search_resources("format:xlsx")
#' }
#' @export
search_resources <- memoise(.search_resources)

#' @export
#' @importFrom tibble as_tibble
#' @aliases Resource
as_tibble.ridl_resources_list <- function(x, ...) {
  l <- lapply(x, as_tibble)
  Reduce(rbind, l)
}

#' @noRd
.pull_resource <- function(identifier, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  res <- configuration$call_action("resource_show",
                                   list(id = identifier))
  RIDLResource$new(initial_data = res,
                   configuration = configuration)
}

#' Pull a RIDL Resource
#'
#' Pull a RIDL Resource
#'
#' @importFrom memoise memoise
#'
#' @param identifier character, a RIDLResource id
#' @param configuration RIDLConfig, the configuration used
#'
#' @rdname pull_resource
#'
#' @return Resource
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL default server
#'  set_ridl_config()
#'  res <- pull_resource("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#'  res
#' }
pull_resource <- memoise(.pull_resource)

#' Create a RIDL resource from list
#'
#' Create a RIDL resource from list with required fields
#'
#' @param initial_data list, list of data
#' @param configuration RIDLConfig, a configuration
#'
#' @return Resource the resource
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  rsdata <- list(name = "hum-resource",
#'                 title = "Humanitarian resource")
#'  res <- ridl_resource(rsdata)
#'  res
#' }
ridl_resource <- function(initial_data, configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    set_ridl_config(configuration = configuration)
  configuration <- get_ridl_config()
  assert_valid_resource_data(initial_data)
  RIDLResource$new(initial_data)
}

#' @rdname browse
#' @export
browse.RIDLResource <- function(x, ...)
  x$browse()
