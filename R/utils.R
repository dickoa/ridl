#' @noRd
is_null_recursive <- function(x)
  is.null(x) | all(vapply(x, is.null, logical(1)))

#' @noRd
drop_nulls <- function(x) {
  x <- Filter(Negate(is_null_recursive), x)
  lapply(x, function(x)
    if (is.list(x)) drop_nulls(x) else x)
}

#' @noRd
merge_list_update <- function(x, y) {
  n1 <- names(x)
  n2 <- names(y)
  n <- union(n1, n2)
  for (field in n) {
    if (field %in% n2)
      x[[field]] <- y[[field]]
  }
  x
}

#' @noRd
merge_list_patch <- function(x, y) {
  n1 <- names(x)
  n2 <- names(y)
  cn <- intersect(n1, n2)
  n <- union(n1, n2)
  for (field in n) {
    if (field %in% n2)
      x[[field]] <- y[[field]]
  }
  x
}

#' @noRd
as_pylog <- function(x) {
  stopifnot(is.logical(x))
  if (x)
    'True'
  else
    'False'
}

#' @noRd
format_size <- function(x) {
  if (!is.null(x)) {
    class(x) <- "object_size"
    x <- format(x, "auto")
  }
  x
}

#' @noRd
hide_token <- function(x) {
  a <- paste(rep("x", 10), collapse = "")
  b <- substr(x, nchar(x) - 10, nchar(x))
  paste(c(a, b), collapse = "")
}

#' @noRd
rbind_tibble <- function(x) {
  nm <- unique(unlist(lapply(x, names)))
  x <- lapply(x, function(df) {
    df[setdiff(nm, names(df))] <- NA
    df
  })
  Reduce(rbind.data.frame, x)
}


#' @noRd
check_config_params <- function(site = c("prod", "test"),
                                token = NULL,
                                key = NULL,
                                user_agent = NULL) {

  if (!is.null(user_agent) && !is.character(user_agent))
    stop("user_agent should be a character",
         call. = FALSE)

  if (site == "prod") {
    site_url <- "https://ridl.unhcr.org"
    token_env <- Sys.getenv("RIDL_API_TOKEN")

    if (is.null(token)) {
          if (!nzchar(token_env))
            warning("You need to properly set the `RIDL_API_TOKEN` variable or use the `token parameter` in the `ridl_config_setup` function!",
                    call. = FALSE)
          token <- token_env
          Sys.setenv("RIDL_API_TOKEN" = token)
    }
  } else {
    site_url <- "https://ridl-uat.unhcr.org"
    token_env <- Sys.getenv("RIDL_UAT_API_TOKEN")

    if (is.null(token)) {
      if (!nzchar(token_env))
        warning("You are using the test server, you need to properly set the `RIDL_UAT_API_TOKEN` variable or use the `token parameter` in the `ridl_config_setup` function!",
                call. = FALSE)
      token <- token_env
      Sys.setenv("RIDL_UAT_API_TOKEN" = token)
    }
  }

  list(token = token, key = key, site_url = site_url)
}

#' @noRd
assert_configuration <- function(configuration)
  if (is.null(configuration) | !inherits(configuration, "RIDLConfig"))
    stop("RIDL configuration not set! Use `set_ridl_config`", call. = FALSE)

#' @noRd
assert_dataset <- function(x) {
  if (!inherits(x, "RIDLDataset"))
    stop("Not an RIDL dataset!", call. = FALSE)
}

#' @noRd
assert_dataset_on_ridl <- function(x) {
  if (!inherits(x, "RIDLDataset") &
        (is.null(x$data$id) | !is_valid_uuid(x$data$id)))
    stop("Not an RIDL dataset or dataset on RIDL!", call. = FALSE)
}

#' @noRd
assert_datasets_list <- function(x) {
  if (!inherits(x, "ridl_datasets_list"))
    stop("Not a list of RIDL Datasets!", call. = FALSE)
  invisible(x)
}

#' @noRd
#' @importFrom stats setNames
ridl_dataset_param_lookup <- function(key, val) {
  l <- .ridl_dataset_lookup_list[[key]]
  x <- names(l)

  lookup <- vapply(unique(val),
                   function(pat)
                     grep(pat,
                          x = x,
                          value = TRUE,
                          ignore.case = TRUE)[1],
                   character(1))

  bool <- vapply(lookup,
                 function(x) is.na(x),
                 logical(1))

  if (any(bool))
    stop(paste0("No match for '",
                paste(val[bool], collapse = ", "), "'"),
         call. = FALSE)

  res <- l[lookup]
  if (length(val) < 2)
    res <- l[[lookup[1]]]
  res
}

#' @noRd
#' @importFrom stats setNames
dataset_fields_choices_val <- function() {
  bool <- vapply(.ridl_dataset_schema$dataset_fields,
                 function(x) "choices" %in% names(x),
                 logical(1))
  param_with_choices <- .ridl_dataset_schema$dataset_fields[bool]
  val <- lapply(param_with_choices,
                function(x)
                  vapply(x$choices,
                         function(cc) cc$value, character(1)))
  par <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
  setNames(val, par)
}

#' @noRd
validate_dataset_data_minimal <- function(x) {
  fields <- RIDLDataset$new()$get_fields()
  nm <- names(x)
  m <- match(nm, fields)
  if (!all(is.na(m)))
    stop("You need at least one RIDL Dataset field to patch",
         call. = FALSE)
  invisible(x)
}

#' @noRd
validate_dataset_data <- function(x) {
  RIDLDataset$new(x)$check_required_fields()
  choices_val <- dataset_fields_choices_val()
  nm <- intersect(names(choices_val), names(x))
  for (n in nm) {
    choices_nm <- tolower(choices_val[[n]])
    if (any(!tolower(x[[n]]) %in% choices_nm)) {
      stop(paste("Field", n, "has",
                 length(choices_nm),
                 "value(s):",
                 paste(choices_nm, collapse = ", ")),
           call. = FALSE)
    }
  }
  x
}

#' @noRd
#' @importFrom stats setNames
resource_fields_choices_val <- function() {
  bool <- vapply(.ridl_dataset_schema$resource_fields,
                 function(x) "choices" %in% names(x),
                 logical(1))
  param_with_choices <- .ridl_dataset_schema$resource_fields[bool]
  val <- lapply(param_with_choices,
                function(x)
                  vapply(x$choices,
                         function(cc) cc$value, character(1)))
  par <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
  setNames(val, par)
}

#' @noRd
validate_resource_data <- function(x) {
  RIDLResource$new(x)$check_required_fields()
  choices_val <- resource_fields_choices_val()
  nm <- intersect(names(choices_val), names(x))
  for (n in nm) {
    choices_nm <- tolower(choices_val[[n]])
    if (any(!tolower(x[[n]]) %in% choices_nm)) {
      stop(paste("Field", n, "has",
                 length(choices_nm),
                 "value(s):",
                 paste(choices_nm, collapse = ", ")),
           call. = FALSE)
    }
  }

  if (x$file_type == "microdata" && x$type != "data")
    stop("If you use file_type='microdata', you also need to use type='data'",
         call. = FALSE)
  x
}

#' @noRd
assert_resource_upload <- function(x) {
  if (!inherits(x, "RIDLResource"))
    stop("Not an RIDL Resource object!", call. = FALSE)
  if (is.null(x$data$upload) | !inherits(x$data$upload, "form_file"))
    stop("Use set_file_to_upload to add a file to upload!",
         call. = FALSE)
  invisible(x)
}

#' @noRd
assert_resource <- function(x) {
  if (!inherits(x, "RIDLResource"))
    stop("Not an RIDL Resource object!", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_resource_on_ridl <- function(x) {
  if (!inherits(x, "RIDLResource") &
        (is.null(x$data$id) | !is_valid_uuid(x$data$id)))
    stop("Not an RIDLResource object or not on RIDL yet!",
         call. = FALSE)
  invisible(x)
}

#' @noRd
assert_resources_list <- function(x) {
  if (!inherits(x, "ridl_resources_list"))
    stop("Not a list of RIDL Resources!", call. = FALSE)
  invisible(x)
}

#' @noRd
validate_container_data <- function(x) {
  RIDLContainer$new(x)$check_required_fields()
  choices_val <- container_fields_choices_val()
  nm <- intersect(names(choices_val), names(x))
  for (n in nm) {
    choices_nm <- tolower(choices_val[[n]])
    if (any(!tolower(x[[n]]) %in% choices_nm)) {
      stop(paste("Field", n, "has",
                 length(choices_nm),
                 "value(s):",
                 paste(choices_nm, collapse = ", ")),
           call. = FALSE)
    }
  }
  x
}

#' @noRd
#' @importFrom stats setNames
container_fields_choices_val <- function() {
  bool <- vapply(.ridl_container_schema$fields,
                 function(x) "choices" %in% names(x),
                 logical(1))
  param_with_choices <- .ridl_container_schema$fields[bool]
  val <- lapply(param_with_choices,
                function(x)
                  vapply(x$choices,
                         function(cc) cc$value, character(1)))
  par <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
  setNames(val, par)
}

#' A dictionnary with the labels and values for container sectoral areas
#'
#' A dictionnary with the labels and values for container sectoral areas
#'
#' @return a data.frame with the keywords labels and values
#' @export
ridl_container_sector <- function() {
  bool <- vapply(.ridl_container_schema$fields,
                 function(x) x$field_name == "sectoral_area",
                 logical(1))
  sector <- .ridl_container_schema$fields[bool]
  value <- unlist(lapply(sector,
                         function(x)
                           vapply(x$choices,
                                  function(cc) cc$value, character(1))))
  label <- unlist(lapply(sector,
                         function(x)
                           vapply(x$choices,
                                  function(cc) cc$label, character(1))))
  data.frame(label = label, value = value)
}

#' A dictionnary with the labels and values for container country
#'
#' A dictionnary with the labels and values for container country
#'
#' @return a data.frame with the keywords labels and values
#' @export
ridl_container_country <- function() {
  bool <- vapply(.ridl_container_schema$fields,
                 function(x) x$field_name == "country",
                 logical(1))
  country <- .ridl_container_schema$fields[bool]
  value <- unlist(lapply(country,
                         function(x)
                           vapply(x$choices,
                                  function(cc) cc$value, character(1))))
  label <- unlist(lapply(country,
                         function(x)
                           vapply(x$choices,
                                  function(cc) cc$label, character(1))))
  data.frame(label = label, value = value)
}

#' @noRd
assert_container <- function(x) {
  if (!inherits(x, "RIDLContainer"))
    stop("Not an RIDL Container object!", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_container_on_ridl <- function(x) {
  if (!inherits(x, "RIDLContainer") &
        (is.null(x$data$id) | !is_valid_uuid(x$data$id)))
    stop("Not an RIDLContainer object or not on RIDL yet!",
         call. = FALSE)
  invisible(x)
}

#' @noRd
assert_container_name <- function(x) {
  l <- ridl_container_list()
  if (!x %in% l)
    stop("Not a valid RIDL container name", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_cache <- function(x)
  if (!inherits(x, "HoardClient"))
    stop("Not a `hoardr` cache object", call. = FALSE)

#' @noRd
assert_memoise_cache <- function(x)
  if (!inherits(x, "cache_mem"))
    stop("Not a `cachem` cache object", call. = FALSE)

#' @noRd
parse_response <- function(res) {
  if(!inherits(res, "HttpResponse"))
    stop("Not a API call response object!", call. = FALSE)
  if (res$status_code < 400) {
    x <- jsonlite::fromJSON(res$parse(encoding = "UTF-8"),
                            simplifyVector = FALSE)
    x <- x$result
  } else {
    x <- list()
  }
  x
}

#' @noRd
check_packages <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  }
}

#' @noRd
`[.ridl_datasets_list` <- function(x, i, ...) {
  structure(NextMethod("["), class = class(x))
}

#' @noRd
`[.ridl_resources_list` <- function(x, i, ...) {
  structure(NextMethod("["), class = class(x))
}

#' @noRd
is_valid_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(regex, x, ignore.case = TRUE)
}

#' @noRd
#' @importFrom utils packageVersion
get_user_agent <- function(x) {
  ridl_version <- packageVersion("ridl")
  os <- Sys.info()[["sysname"]]
  os_version <- paste(Sys.info()[["release"]], Sys.info()[["version"]])
  r_version <- paste0(R.version$major, ".",
                      R.version$minor,
                      ifelse(R.version$status == "", "",
                             paste0("-", R.version$status)))
  header <- paste0("ridl/", ridl_version, " (", os, "/",
                   os_version, "; ", "R/", r_version, ")")
  header
}

#' @noRd
#' @author Dirk Schumascher
find_schema_row <- function(tbl) {
  stopifnot(is.data.frame(tbl))
  if (any(is_valid_tag(colnames(tbl)))) {
    return(0)
  } else {
    for (i in seq_len(pmin(nrow(tbl), 25))) {
      row <- unlist(apply(tbl[i, ], 2, as.character))
      if (any(is_valid_tag(row))) {
        return(i)
      }
    }
  }
  -1
}

#' Strip HXL tags from tibble
#'
#' Strip HXL tags from tibble
#' @importFrom readr type_convert
#' @param x a tibble with HXL tags
#' @return tibble
#' @noRd
strip_hxl <- function(x) {
  tbl <- tibble::as_tibble(x)
  schema_row <- find_schema_row(tbl)
  base_tbl <- if (schema_row > 0) {
    new_tbl <- tbl[-1 * 1L:schema_row, ]
    suppressMessages(type_convert(new_tbl))
  } else {
    tbl
  }
  base_tbl
}

#' @noRd
#' @author Dirk Schumascher
is_valid_tag <- function(tag) {
  ltag <- tolower(trimws(tag))
  pattern <- "^#[a-z][a-z0-9_]*(\\s+(\\+|-)\\s*[a-z][a-z0-9_]*)*"
  grepl(x = ltag, pattern = pattern)
}

#' @importFrom readr read_delim default_locale locale
#' @noRd
read_ridl_delim_ <- function(file, delim = ",", locale = default_locale(), hxl = FALSE, ...) {
  check_packages("readr")
  df <- read_delim(file, delim = delim, locale = locale, ...)
  if (isTRUE(hxl))
    df <- strip_hxl(df)
  df
}

#' @importFrom readxl excel_sheets read_excel
#' @noRd
read_ridl_excel_ <- function(file = NULL, sheet = NULL, hxl = FALSE, ...) {
  check_packages("readxl")
  if (is.null(sheet)) {
    sheet <- excel_sheets(file)[[1]]
    message("Reading sheet: ", sheet, "\n")
  }
  df <- read_excel(file, sheet = sheet, ...)
  if (isTRUE(hxl))
    df <- strip_hxl(df)
  df
}

#' @importFrom haven read_sav
#' @noRd
read_ridl_spss_ <- function(file, ...) {
  check_packages("haven")
  read_sav(file, ...)
}

#' @importFrom haven read_dta
#' @noRd
read_ridl_stata_ <- function(file, ...) {
  check_packages("haven")
  read_dta(file, ...)
}

#' @importFrom readxl excel_sheets
#' @noRd
ridl_resource_sheets_ <- function(file = NULL) {
  check_packages("readxl")
  excel_sheets(file)
}

#' Encode URL from proxy.hxlstandard
#'
#' URL using are partially encoded we need to change space into %20
#'
#' @return Character encoded url
#' @noRd
url_encode_proxy <- function(url)
  gsub("\\s", "%20", url)

#' @noRd
#' @param z object to display
#' inspired by Scott Chamberlain function sift_res
#' @importFrom stats na.omit
sift_res <- function(z, key = "name", n = 5) {
  if (!is.null(z) && length(z) > 0) {
    if (!key %in% names(z)) key <- "name"
    r <- na.omit(vapply(z,
                        function(x) if (length(x) > 0)
                          paste0(x[[key]], ", ") else "",
                        FUN.VALUE = "character")[1:n])
    gsub(", $", "", paste0(r, collapse = ""))
  } else {
    ""
  }
}

#' @noRd
#' @importFrom logger log_error log_info
log_response <- function(res) {
  if (!nzchar(Sys.getenv("RIDL_LOG"))) {
    times <- res$times[res$times > 0]

    msg <- sprintf(fmt = "STATUS %s - %s - [%s]",
      res$status_code,
      res$url,
      paste0(sprintf("%s:%f",
                     names(times), times),
             collapse = ", ")
    )

    if (res$status_code >= 400) {
      msg <- paste(msg, rawToChar(res$content),
                   sep = " - ")
      logger::log_error("%s", msg)
    } else {
      logger::log_info("%s", msg)
    }
  }
}


#' @noRd
#' @importFrom logger log_info
log_request <- function(req) {
  if (!!nzchar(Sys.getenv("RIDL_LOG"))) {
    log_info(fmt = "%s %s",
             toupper(req$method),
             req$url$url)
  }
}

#' @noRd
dataset_name_ <- function(ids) {
  vapply(ids, function(id) {
    rd_show(id)$data$name
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd
keyword_lookup_ <- function(x) {
  unname(.ridl_dataset_lookup_list$keywords[x])
}
