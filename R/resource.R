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
        private$configuration <- ridl_config_get()
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

      con <- ridl_config_get()
      cli <- con$remoteclient()

      url <- self$data$url

      url <- gsub(con$data$site_url,
                  "",
                  url)

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
    read = function(sheet = NULL, format = NULL,
                    download_folder = NULL,
                    force_download = FALSE,
                    quiet_download = TRUE, ...) {

      if (!is.null(private$download_folder_) & is.null(download_folder))
        folder <- self$download_folder()

      file_path <- self$download(folder = download_folder,
                                 quiet = quiet_download,
                                 force = force_download)

      if (is.null(format))
        format <- self$file_format()

      hxl <- tolower(self$data$`hxl-ated`) != "true"

      switch(format,
             csv = read_ridl_delim_(file_path,
                                   hxl = hxl, ...),
             xlsx = read_ridl_excel_(file_path, sheet = sheet,
                                    hxl = hxl, ...),
             xls = read_ridl_excel_(file_path, sheet = sheet,
                                   hxl = hxl, ...),
             `.csv` = read_ridl_delim_(file_path,
                                      hxl = hxl, ...),
             `.xlsx` = read_ridl_excel_(file_path, sheet = sheet,
                                       hxl = hxl, ...),
             `.xls` = read_ridl_excel_(file_path, sheet = sheet,
                                      hxl = hxl, ...),
             stata = read_ridl_stata_(file_path, ...),
             dta = read_ridl_stata_(file_path, ...),
             `.dta` = read_ridl_stata_(file_path, ...),
             sav = read_ridl_spss_(file_path, ...),
             `.sav` = read_ridl_spss_(file_path, ...),
             spss = read_ridl_spss_(file_path, ...))
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
    excel_sheets = function(format = NULL, download_folder = NULL,
                          quiet_download = TRUE, force_download = FALSE) {

      if (!is.null(private$download_folder_) & is.null(download_folder))
        folder <- self$download_folder()

      file_path <- self$download(folder = download_folder,
                                 quiet = quiet,
                                 force = force_download)

      if (is.null(format))
      format <- self$file_format()

      if (!format %in% c("xlsx", "xls"))
        stop("`ridl_resource_excel_sheets works only with Excel file",
             call. = FALSE)

      switch(format,
             xlsx = ridl_resource_sheets_(file_path),
             xls = ridl_resource_sheets_(file_path))
    },

    #' @description
    #' Get the resource dataset.
    #'
    #' @return a RIDLDataset, the dataset containing the resource
    ridl_dataset = function() {
      dataset_id <- self$data$package_id
      if (is.null(dataset_id)) {
        stop("Resource has no dataset id!", call. = FALSE)
      } else {
        ridl_dataset_show(dataset_id)
      }
    },

    #' @description
    #' Get the file format
    #' @return a character, the file format of the resource
    file_format = function() {
      tolower(self$data$format)
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
    #' Get the file that will be or has been uploaded if any
    #'
    #' @return the file to upload
    get_file_to_upload = function() {
      self$data$file_to_upload
    },

    #' @description
    #' Set the file that will be uploaded
    #'
    #' @importFrom crul upload
    #' @importFrom tools file_ext
    #'
    #' @param file_to_upload character the path to the file to upload
    set_file_to_upload = function(file_to_upload) {
      if ("url" %in% names(self$data))
        self$data$url <- NULL

      self$data$file_to_upload <- file_to_upload
      self$data$url_type <- "upload"
      self$data$upload <- upload(file_to_upload)
      if (is.null(self$data$format))
        self$data$format <- file_ext(basename(file_to_upload))
      self$data$size <- file.size(file_to_upload)
    },

    #' @description
    #' Check if the resource has an url or a file to upload but not both
    #'
    #' @return a logical value, TRUE if the the resource don't mix 'url' and
    #' and 'file_to_upload'
    check_resource_type = function() {
      nm <- names(self$data)
      bool1 <- "url" %in% nm & "file_to_upload" %in% nm
      bool2 <- !"url" %in% nm & !"file_to_upload" %in% nm
      if (bool1)
        stop("You can't use a url and file to upload simultaneously",
             call. = FALSE)
      if (bool2)
        stop( "You have to use either a url a file to upload",
             call. = FALSE)
      TRUE
    },

    #' @description
    #' Get resource fields
    #'
    #' @return list of fields for a resource
    get_fields = function() {
      url <- private$configuration$get_site_url()
      fields <- .ridl_dataset_schema$resource_fields
      if (grepl("uat", url))
        fields <- .ridl_dataset_uat_schema$resource_fields
      nm <- vapply(fields,
                   function(x) x$field_name, character(1))
      nm
    },

    #' @description
    #' Get resource required fields
    #'
    #' @return list of required fields for a resource
    get_required_fields = function() {
      url <- private$configuration$get_site_url()
      fields <- .ridl_dataset_schema$resource_fields
      if (grepl("uat", url))
        fields <- .ridl_dataset_uat_schema$resource_fields
      nm <- self$get_fields()
      bool <- lapply(fields,
                  function(x) x$required)
      bool <- vapply(bool, isTRUE, logical(1))
      nm[bool]
    },

    #' @description
    #' Check resource required field
    #'
    #' @return a logical value, TRUE if the the resource
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (!all(n1 %in% n2)) {
        stop(sprintf("Field %s is missing in the RIDLResource object!\n",
                     setdiff(n1, n2)),
             call. = FALSE)
      } else {
        TRUE
      }
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
    ridl_browse = function() {
      url <- private$configuration$get_site_url()
      dataset_id <- self$data$package_id
      resource_id <- self$data$id
      browseURL(url = paste0(url, "/dataset/", dataset_id,
                             "/resource/", resource_id))
    },

    #' @description
    #' Print a Resource object
    print = function() {
      cat(paste0("<RIDL Resource> ", self$data[["id"]]), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$description, "\n", sep = "")
      cat("  Type: ", self$data$file_type, "\n", sep = "")
      cat("  Size: ", format_size(self$data$size), "\n", sep = "")
      cat("  Format: ", tolower(self$data$format), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases RIDLResource
#' @importFrom tibble as_tibble
as_tibble.RIDLResource <- function(x, ...) {
  data <- x$data
  fields <- x$get_fields()
  data <- drop_nulls(data[fields])
  data <- as_tibble(data)
  data$resource_url <- data$url
  data$resource <- list(x)
  data
}

#' @export
#' @aliases RIDLResource
as_tibble.ridl_resource_list <- function(x) {
  l <- lapply(x, as_tibble)
  rbind_tibble(l)
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
#' @rdname ridl_resource_download
#'
#' @examples
#' \dontrun{
#' #Setting the config to use RIDL default server
#'  res <- ridl_resource_show("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#'  ridl_resource_download(res, folder = "/tmp")
#' }
ridl_resource_download.RIDLResource <- function(resource,
                                                folder = NULL,
                                                filename = NULL,
                                                quiet = FALSE,
                                                force = FALSE, ...) {

  resource$download(folder = folder, filename = filename,
                    quiet = quiet, force = force, ...)
}

#' @rdname ridl_resource_download
#' @export
rr_download.RIDLResource <- ridl_resource_download.RIDLResource

#' Get the names of the sheets of XLS(X) resources
#'
#' Get the names of the sheets of XLS(X) resources
#'
#' @param resource RIDLResource, a RIDL resource
#' @param format character, specify file format in case the automatic reader doesn't work as expected
#' @param download_folder character, path of the directory where you will store the data
#' @param quiet logical, no progress bar from download (default = FALSE)
#'
#' @rdname ridl_resource_excel_sheets
#'
#' @return the names of the sheets of XLS(X) resources
#'
#' @export
ridl_resource_excel_sheets.RIDLResource <- function(resource,
                                                    format = NULL,
                                                    download_folder = NULL,
                                                    quiet = TRUE) {
  resource$excel_sheets(format = format,
                        download_folder = download_folder,
                        quiet = quiet)
}

#' @rdname ridl_resource_excel_sheets
#' @export
rr_excel_sheets.RIDLResource <- ridl_resource_excel_sheets.RIDLResource

#' Get the file format of the resource
#'
#' Get the file format of the resource
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname ridl_resource_file_format
#'
#' @return A character, the format of the resource
#'
#' @export
ridl_resource_file_format.RIDLResource <- function(resource) {
  resource$file_format()
}

#' @rdname ridl_resource_file_format
#' @export
rr_file_format.RIDLResource <- ridl_resource_file_format.RIDLResource

#' Get and set the file to upload
#'
#' Get and set the file to upload
#'
#' @param file_to_upload character the path to the file to upload
#' @param value character, the path of the file to upload
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname ridl_resource_file_to_upload
#'
#' @return A RIDLResource with a file
#' @export
ridl_resource_file_to_upload_set.RIDLResource <- function(resource, file_to_upload) {
  resource$set_file_to_upload(file_to_upload)
  invisible(resource)
}

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_set.RIDLResource <- ridl_resource_file_to_upload_set.RIDLResource

#' @rdname ridl_resource_file_to_upload
#' @export
`ridl_resource_file_to_upload<-.RIDLResource` <- function(resource, value) {
  resource$set_file_to_upload(value)
  invisible(resource)
}

#' @rdname ridl_resource_file_to_upload
#' @export
`rr_file_to_upload<-.RIDLResource` <- `ridl_resource_file_to_upload<-.RIDLResource`

#' Get the file to upload
#'
#' Get the file to upload
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname ridl_resource_file_to_upload
#'
#' @return A character, the path to file to upload or NULL if not available
#' @export
ridl_resource_file_to_upload_get.RIDLResource <- function(resource) {
  resource$get_file_to_upload()
}

#' @rdname ridl_resource_file_to_upload
#' @export
rr_file_to_upload_get.RIDLResource <- ridl_resource_file_to_upload_get.RIDLResource

#' Get the dataset containing the resource
#'
#' @param resource RIDLResource, a RIDL resource
#'
#' @rdname ridl_resource_dataset
#'
#' @return a RIDLDataset, the dataset containing the resource
#'
#' @export
ridl_resource_dataset.RIDLResource <- function(resource) {
  resource$ridl_dataset_get()
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
#'
#' @rdname ridl_resource_read
#'
#' @return A \code{tibble}
#'
#' @export
ridl_resource_read.RIDLResource <- function(resource,
                                            sheet = NULL,
                                            format = NULL,
                                            download_folder = NULL,
                                            force_download = FALSE,
                                            quiet_download = TRUE, ...) {
  resource$read(sheet = sheet,
                format = format,
                download_folder = download_folder,
                force_download = force_download,
                quiet_download = quiet_download,
                ...)
}

#' @rdname ridl_resource_read
#' @export
rr_read.RIDLResource <- ridl_resource_read.RIDLResource

#' Search for RIDL resources
#'
#' Search for RIDL resources
#'
#'
#' @param query character, a query
#' @param configuration RIDLConfig, a configuration
#' @param ... extra params
#'
#' @details Search and find datasets on RIDL
#'
#' @return A list of RIDLResource
#'
#' @rdname ridl_resource_search
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL default server
#'  search_ridl_resource("format:xlsx")
#' }
#'
#' @export
ridl_resource_search <- function(query = "*:*", configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  res <- configuration$call_action("resource_search",
                                   list(query = query, ...))
  res$raw$raise_for_status()
  list_of_rs <- lapply(res$result$results, function(x)
    RIDLResource$new(initial_data = x,
                     configuration = configuration))
  class(list_of_rs) <- "ridl_resource_list"
  list_of_rs
}

#' @rdname ridl_resource_search
#' @export
rr_search <- ridl_resource_search

#' @importFrom tibble as_tibble
#' @aliases RIDLResource
#' @export
as_tibble.ridl_resource_list <- function(x, ...) {
  l <- lapply(x, as_tibble)
  Reduce(rbind, l)
}

#' Pull a RIDL resource
#'
#' Pull a RIDL resource
#'
#' @param identifier character, a RIDLResource id
#' @param configuration RIDLConfig, the configuration used
#'
#' @rdname ridl_resource_show
#'
#' @return RIDLResource
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use RIDL default server
#'  ridl_config_set()
#'  res <- ridl_resource_show("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#'  res
#' }
ridl_resource_show <- function(identifier, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()
  res <- configuration$call_action("resource_show",
                                   list(id = identifier))
  res$raw$raise_for_status()
  RIDLResource$new(initial_data = res$result,
                   configuration = configuration)
}

#' @rdname ridl_resource_show
#' @export
rr_show <- ridl_resource_show

#' Create a RIDL resource from list
#'
#' Create a RIDL resource from list with required fields
#'
#' @param type character, Resource type(*) - The kind of file you want to upload. Allowed values: `data` (Data file), `attachment` (Additional attachment).
#' @param url character, Upload - The file name as it will be recorded in the system.
#' @param description character, Description - Some usefule notes about the data.
#' @param format character, File format - eg. CSV, XML, or JSON.
#' @param file_type character, File type(*) - Indicates what is contained in the file. Allowed values: `microdata` (Microdata), `questionnaire` (Questionnaire), `report` (Report), `sampling_methodology` (Sampling strategy & methodology Description), `infographics` (Infographics & Dashboard), `script` (Script), `concept note` (Concept Note), `other` (Other).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param measurement_unit character, the unit of measurement
#' @param methodology character, This element documents methodological details on the production of the series or indicator.
#' @param file_to_upload character, path of the file to upload
#' @param date_range_start Date, Data collection first date(*) - Use yyyy-mm-dd format.
#' @param date_range_end Date, Data collection last date(*) - Use yyyy-mm-dd format.
#' @param version character, Version(*).
#' @param hxlated logical, HXL-ated. Allowed values: `False` (No), `True` (Yes).
#' @param process_status character, File process status(*) - Indicates the processing stage of the data. 'Raw' means that the data has not been cleaned since collection. 'In process' means that it is being cleaned. 'Final' means that the dataset is final and ready for use in analytical products. Allowed valued: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability(*) - Indicates if personally identifiable data is contained in the dataset. Allowed values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3st level: Public Use File (PUF)).
#' @param script_field_name character, name of the script field
#' @param script_software character, software used to run the script
#' @param script_methods character, statistical/analytic methods included in the script
#' @param script_instructions character, instructions for running the script. Information on the sequence in which the scripts must be run is critical.
#' @param script_dependencies character, dependencies (packages/libraries) that the script relies on.
#' @param script_zip_package character, Script ZIP package
#' @param license_name character, script license name
#' @param license_uri character, script license uri
#' @param source_code_repo character, repository (e.g. GitHub repo) where the script has been published.
#' @param script_date, character, date of the script
#' @param authors character, authors
#' @param dimensions character, dimensions
#' @param statistical_concept character, statistical concept used
#' @param time_periods character, time periods
#' @param confidentiality character, confidentiality
#' @param confidentiality_status character, confidentiality status
#' @param confidentiality_note character, confidentiality note
#' @param limitations character, limitations
#' @param periodicity character, periodicity
#' @param kobo_type character, type
#' @param kobo_details character, details
#' @param name character, the name of the resource
#' @param configuration RIDLConfig, the configuration
#'
#' @return Resource the resources
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  res <- ridl_resource(type = "microdata",
#'                       file_type = "csv")
#'  res
#' }
ridl_resource <- function(type,
                          file_type,
                          date_range_start,
                          date_range_end,
                          version,
                          process_status,
                          identifiability,
                          visibility,
                          measurement_unit,
                          methodology,
                          file_to_upload = NULL,
                          url = NULL,
                          name = NULL,
                          description = NULL,
                          format = NULL,
                          hxlated = NULL,
                          authors = NULL,
                          script_field_name = NULL,
                          script_methods = NULL,
                          script_instructions = NULL,
                          script_dependencies = NULL,
                          script_zip_package = NULL,
                          script_date = NULL,
                          license_name = NULL,
                          license_uri = NULL,
                          limitations = NULL,
                          dimensions = NULL,
                          statistical_concept = NULL,
                          time_periods = NULL,
                          confidentiality = NULL,
                          confidentiality_status = NULL,
                          confidentiality_note = NULL,
                          script_software = NULL,
                          source_code_repo = NULL,
                          periodicity = NULL,
                          kobo_type = NULL,
                          kobo_details = NULL,
                          configuration = NULL) {
  if (!is.null(configuration) &  inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)
  configuration <- ridl_config_get()

  data <- list(type = type,
               url = url,
               name = name,
               description = description,
               format = format,
               file_type = file_type,
               measurement_unit = measurement_unit,
               methodology = methodology,
               date_range_start = date_range_start,
               date_range_end = date_range_end,
               version = version,
               `hxl-ated` = hxlated,
               process_status = process_status,
               identifiability = identifiability,
               authors = authors,
               script_field_name = script_field_name,
               script_methods = script_methods,
               script_instructions = script_instructions,
               script_dependencies = script_dependencies,
               script_zip_package = script_zip_package,
               script_date = script_date,
               license_name = license_name,
               license_uri = license_uri,
               limitations = limitations,
               dimensions = dimensions,
               statistical_concept = statistical_concept,
               time_periods = time_periods,
               confidentiality = confidentiality,
               confidentiality_status = confidentiality_status,
               confidentiality_note = confidentiality_note,
               script_software = script_software,
               source_code_repo = source_code_repo,
               periodicity = periodicity,
               visibility = visibility,
               kobo_type = kobo_type,
               kobo_details = kobo_details)
  data <- drop_nulls(data)

  data <- validate_resource_data(data)
  rs <- RIDLResource$new(data,
                         configuration)

  if (!is.null(file_to_upload))
    rs <- rs$set_file_to_upload(file_to_upload)
  rs
}

#' @rdname ridl_browse
#' @export
ridl_browse.RIDLResource <- function(x, ...)
  x$ridl_browse()

#' Create a resource on RIDL
#'
#' Create a resource on RIDL
#'
#' @param resource RIDLResource, a resource object
#' @param dataset RIDLDataset, the dataset where you can share the dataset
#' @param configuration RIDLConfig, the configuration
#'
#' @rdname ridl_resource_create
#'
#' @return RIDLResource, the resource
#' @export
ridl_resource_create <- function(resource,
                                 dataset,
                                 configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_resource(resource)
  assert_dataset_on_ridl(dataset)
  resource$check_resource_type()

  data <- resource$data
  data$package_id <- dataset$data$id

  encode <- if (is.null(data$upload)) "json" else "multipart"

  res <- configuration$call_action("resource_create",
                                   body = data,
                                   verb = "post",
                                   encode = encode)

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_resource_create
#' @export
rr_create <- ridl_resource_create

#' Update a resource on RIDL
#'
#' Update a resource on RIDL
#'
#' @param resource RIDLResource, the resource to update
#' @param type character, Resource type(*) - The kind of file you want to upload. Allowed values: `data` (Data file), `attachment` (Additional attachment).
#' @param url character, Upload - The file name as it will be recorded in the system.
#' @param description character, Description - Some usefule notes about the data.
#' @param format character, File format - eg. CSV, XML, or JSON.
#' @param file_type character, File type(*) - Indicates what is contained in the file. Allowed values: `microdata` (Microdata), `questionnaire` (Questionnaire), `report` (Report), `sampling_methodology` (Sampling strategy & methodology Description), `infographics` (Infographics & Dashboard), `script` (Script), `concept note` (Concept Note), `other` (Other).
#' @param measurement_unit character, the unit of measurement
#' @param methodology character, This element documents methodological details on the production of the series or indicator.
#' @param file_to_upload character, path of the file to upload
#' @param date_range_start Date, Data collection first date(*) - Use yyyy-mm-dd format.
#' @param date_range_end Date, Data collection last date(*) - Use yyyy-mm-dd format.
#' @param version character, Version(*).
#' @param hxlated logical, HXL-ated. Allowed values: `False` (No), `True` (Yes).
#' @param process_status character, File process status(*) - Indicates the processing stage of the data. 'Raw' means that the data has not been cleaned since collection. 'In process' means that it is being cleaned. 'Final' means that the dataset is final and ready for use in analytical products. Allowed valued: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability(*) - Indicates if personally identifiable data is contained in the dataset. Allowed values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3st level: Public Use File (PUF)).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param script_field_name character, name of the script field
#' @param script_software character, software used to run the script
#' @param script_methods character, statistical/analytic methods included in the script
#' @param script_instructions character, instructions for running the script. Information on the sequence in which the scripts must be run is critical.
#' @param script_dependencies character, dependencies (packages/libraries) that the script relies on.
#' @param script_zip_package character, Script ZIP package
#' @param license_name character, script license name
#' @param license_uri character, script license uri
#' @param source_code_repo character, repository (e.g. GitHub repo) where the script has been published.
#' @param script_date, character, date of the script
#' @param authors character, authors
#' @param kobo_type character, type
#' @param kobo_details character, details
#' @param dimensions character, dimensions
#' @param statistical_concept character, statistical concept used
#' @param time_periods character, time periods
#' @param confidentiality character, confidentiality
#' @param confidentiality_status character, confidentiality status
#' @param confidentiality_note character, confidentiality note
#' @param limitations character, limitations
#' @param periodicity character, periodicity
#' @param name character, the name of the resource
#' @param configuration RIDLConfig, the configuration
#'
#' @return RIDLResource, the resource
#' @export
ridl_resource_update <- function(resource,
                                 type,
                                 file_type,
                                 date_range_start,
                                 date_range_end,
                                 version,
                                 process_status,
                                 identifiability,
                                 visibility,
                                 methodology = NULL,
                                 measurement_unit = NULL,
                                 file_to_upload = NULL,
                                 url = NULL,
                                 name = NULL,
                                 description = NULL,
                                 format = NULL,
                                 hxlated = NULL,
                                 authors = NULL,
                                 script_field_name = NULL,
                                 script_methods = NULL,
                                 script_instructions = NULL,
                                 script_dependencies = NULL,
                                 script_zip_package = NULL,
                                 script_date = NULL,
                                 license_name = NULL,
                                 license_uri = NULL,
                                 periodicity = NULL,
                                 limitations = NULL,
                                 dimensions = NULL,
                                 statistical_concept = NULL,
                                 time_periods = NULL,
                                 confidentiality = NULL,
                                 confidentiality_status = NULL,
                                 confidentiality_note = NULL,
                                 script_software = NULL,
                                 source_code_repo = NULL,
                                 kobo_type = NULL,
                                 kobo_details = NULL,
                                 configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_resource_on_ridl(resource)

  data <- list(type = type,
               url = url,
               name = name,
               description = description,
               format = format,
               file_type = file_type,
               measurement_unit = measurement_unit,
               methodology = methodology,
               date_range_start = date_range_start,
               date_range_end = date_range_end,
               version = version,
               `hxl-ated` = hxlated,
               process_status = process_status,
               identifiability = identifiability,
               visibility = visibility,
               authors = authors,
               script_field_name = script_field_name,
               script_methods = script_methods,
               script_instructions = script_instructions,
               script_dependencies = script_dependencies,
               script_zip_package = script_zip_package,
               script_date = script_date,
               license_name = license_name,
               license_uri = license_uri,
               limitations = limitations,
               dimensions = dimensions,
               statistical_concept = statistical_concept,
               time_periods = time_periods,
               confidentiality = confidentiality,
               confidentiality_status = confidentiality_status,
               confidentiality_note = confidentiality_note,
               script_software = script_software,
               source_code_repo = source_code_repo,
               periodicity = periodicity,
               kobo_type = kobo_type,
               kobo_details = kobo_details,
               id = resource$data$id)
  data <- drop_nulls(data)
  data <- validate_resource_data(data)

  encode <- if (is.null(data$upload)) "json" else "multipart"

  res <- configuration$call_action("resource_update",
                                   body = data,
                                   verb = encode)

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_resource_update
#' @export
rr_update <- ridl_resource_update

#' Patch a resource on RIDL
#'
#' Patch a resource on RIDL
#'
#' @param resource RIDLResource, the resource to patch
#' @param type character, Resource type(*) - The kind of file you want to upload. Allowed values: `data` (Data file), `attachment` (Additional attachment).
#' @param url character, Upload - The file name as it will be recorded in the system.
#' @param description character, Description - Some usefule notes about the data.
#' @param format character, File format - eg. CSV, XML, or JSON.
#' @param file_type character, File type(*) - Indicates what is contained in the file. Allowed values: `microdata` (Microdata), `questionnaire` (Questionnaire), `report` (Report), `sampling_methodology` (Sampling strategy & methodology Description), `infographics` (Infographics & Dashboard), `script` (Script), `concept note` (Concept Note), `other` (Other).
#' @param measurement_unit character, the unit of measurement
#' @param methodology character, This element documents methodological details on the production of the series or indicator.
#' @param file_to_upload character, path of the file to upload
#' @param date_range_start Date, Data collection first date(*) - Use yyyy-mm-dd format.
#' @param date_range_end Date, Data collection last date(*) - Use yyyy-mm-dd format.
#' @param version character, Version(*).
#' @param hxlated logical, HXL-ated. Allowed values: `False` (No), `True` (Yes).
#' @param process_status character, File process status(*) - Indicates the processing stage of the data. 'Raw' means that the data has not been cleaned since collection. 'In process' means that it is being cleaned. 'Final' means that the dataset is final and ready for use in analytical products. Allowed valued: `raw` (Raw-Uncleaned), `cleaned` (Cleaned Only), `anonymized` (Cleaned & Anonymized).
#' @param identifiability character, Identifiability(*) - Indicates if personally identifiable data is contained in the dataset. Allowed values: `personally_identifiable` (Personally identifiable), `anonymized_enclave` (Anonymized 1st level: Data Enclave - only removed direct identifiers), `anonymized_scientific` (Anonymized 2st level: Scientific Use File (SUF)), `anonymized_public` (Anonymized 3st level: Public Use File (PUF)).
#' @param visibility character, Internal Access Level(*). Allowed values: `restricted` (Private), `public` (Internally Visible).
#' @param script_field_name character, name of the script field
#' @param script_software character, software used to run the script
#' @param script_methods character, statistical/analytic methods included in the script
#' @param script_instructions character, instructions for running the script. Information on the sequence in which the scripts must be run is critical.
#' @param script_dependencies character, dependencies (packages/libraries) that the script relies on.
#' @param script_zip_package character, Script ZIP package
#' @param license_name character, script license name
#' @param license_uri character, script license uri
#' @param source_code_repo character, repository (e.g. GitHub repo) where the script has been published.
#' @param script_date, character, date of the script
#' @param authors character, authors
#' @param dimensions character, dimensions
#' @param statistical_concept character, statistical concept used
#' @param time_periods character, time periods
#' @param confidentiality character, confidentiality
#' @param confidentiality_status character, confidentiality status
#' @param confidentiality_note character, confidentiality note
#' @param limitations character, limitations
#' @param periodicity character, periodicity
#' @param kobo_type character, type
#' @param kobo_details character, details
#' @param name character, the name of the resource
#' @param configuration RIDLConfig, the configuration
#'
#' @return RIDLResource, the resource
#' @export
ridl_resource_patch <- function(resource,
                                type = NULL,
                                file_type = NULL,
                                measurement_unit = NULL,
                                methodology = NULL,
                                date_range_start = NULL,
                                date_range_end = NULL,
                                version = NULL,
                                process_status = NULL,
                                identifiability = NULL,
                                file_to_upload = NULL,
                                url = NULL,
                                name = NULL,
                                description = NULL,
                                format = NULL,
                                hxlated = NULL,
                                authors = NULL,
                                script_field_name = NULL,
                                script_methods = NULL,
                                script_instructions = NULL,
                                script_dependencies = NULL,
                                script_zip_package = NULL,
                                script_date = NULL,
                                license_name = NULL,
                                license_uri = NULL,
                                visibility = NULL,
                                limitations = NULL,
                                dimensions = NULL,
                                statistical_concept = NULL,
                                time_periods = NULL,
                                confidentiality = NULL,
                                confidentiality_status = NULL,
                                confidentiality_note = NULL,
                                script_software = NULL,
                                source_code_repo = NULL,
                                periodicity = NULL,
                                kobo_type = NULL,
                                kobo_details = NULL,
                                configuration = NULL) {

  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  configuration <- ridl_config_get()
  assert_resource_on_ridl(resource)

  data <- list(type = type,
               url = url,
               name = name,
               description = description,
               format = format,
               file_type = file_type,
               measurement_unit = measurement_unit,
               methodology = methodology,
               date_range_start = date_range_start,
               date_range_end = date_range_end,
               version = version,
               `hxl-ated` = hxlated,
               process_status = process_status,
               identifiability = identifiability,
               visibility = visibility,
               authors = authors,
               script_field_name = script_field_name,
               script_methods = script_methods,
               script_instructions = script_instructions,
               script_dependencies = script_dependencies,
               script_zip_package = script_zip_package,
               script_date = script_date,
               license_name = license_name,
               license_uri = license_uri,
               limitations = limitations,
               dimensions = dimensions,
               statistical_concept = statistical_concept,
               time_periods = time_periods,
               confidentiality = confidentiality,
               confidentiality_status = confidentiality_status,
               confidentiality_note = confidentiality_note,
               script_software = script_software,
               source_code_repo = source_code_repo,
               periodicity = periodicity,
               kobo_type = kobo_type,
               kobo_details = kobo_details,
               id = resource$data$id)
  data <- drop_nulls(data)

  encode <- if (is.null(data$upload)) "json" else "multipart"

  res <- configuration$call_action("resource_patch",
                                   body = data,
                                   verb = "post",
                                   encode = encode)

  res$raw$raise_for_status()
  invisible(res)
}

#' @rdname ridl_resource_patch
#' @export
rr_patch <- ridl_resource_patch

#' @rdname ridl_clone
#' @export
ridl_clone.RIDLResource <- function(x, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

  resource <- x
  configuration <- ridl_config_get()
  assert_resource(resource)
  data <- resource$data
  nm <- vapply(.ridl_dataset_schema$resource_fields,
               function(x) x$field_name, character(1))
  data <- data[nm]
  data$url <- NULL

  RIDLResource$new(data,
                   configuration = configuration)
}

#' Check if a resource id is available on RIDL
#'
#' Check if a resource id is available on RIDL
#'
#' @param resource_id character, the id of the RIDLResource
#' @param configuration RIDLConfig, the RIDL configuration
#'
#' @return A logical value, TRUE if the RIDLResource exists
#' @export
ridl_resource_exist <- function(resource_id, configuration = NULL) {
    if (!is.null(configuration) & inherits(configuration, "RIDLConfig"))
    ridl_config_set(configuration = configuration)

    res <- tryCatch({ridl_resource_show(resource_id);TRUE},
                    error = function(e) {
                      FALSE
                  })
    res
}

#' @rdname ridl_resource_exist
#' @export
rr_exist <- ridl_resource_exist

#' @export
`[.ridl_resource_list` <- function(x, i) {
   structure(NextMethod(), class = "ridl_resource_list")
}
