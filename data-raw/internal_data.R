library(jsonlite)

## URL
dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/develop/ckanext/unhcr/schemas/dataset.json"
data_container_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/develop/ckanext/unhcr/schemas/data_container.json"
deposited_dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/develop/ckanext/unhcr/schemas/deposited_dataset.json"

.ridl_dataset_schema <- fromJSON(dataset_json,
                                  simplifyVector = FALSE)
.ridl_deposited_dataset_schema <- fromJSON(deposited_dataset_json,
                                           simplifyVector = FALSE)
.ridl_container_schema <<- fromJSON(data_container_json,
                                    simplifyVector = FALSE)

### Dataset
bool <- vapply(.ridl_dataset_schema$dataset_fields,
               function(x) "choices" %in% names(x),
               logical(1))

param_with_choices <- .ridl_dataset_schema$dataset_fields[bool]
value <- lapply(param_with_choices,
                function(x)
                  setNames(lapply(x$choices, "[[", "value"),
                           vapply(x$choices,
                                  function(cc) gsub(" / ", "/", tolower(cc$label)),
                                    character(1))))
param <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
.ridl_dataset_lookup_list <- setNames(value, param)

lookup_rd_list_to_df <- function(key) {
  df <- tibble::tibble(code = unlist(.ridl_dataset_lookup_list[[key]]),
                       label = names(.ridl_dataset_lookup_list[[key]]))
  names(df) <- paste0(key, "_", names(df))
  readr::type_convert(df)
}

nm <- names(.ridl_dataset_lookup_list)
all_df <- lapply(names(.ridl_dataset_lookup_list), lookup_rd_list_to_df)
names(all_df) <- paste0("rd_metadata_", nm)
list2env(all_df, globalenv())


### Resource
bool <- vapply(.ridl_dataset_schema$resource_fields,
               function(x) "choices" %in% names(x),
               logical(1))

param_with_choices <- .ridl_dataset_schema$resource_fields[bool]
value <- lapply(param_with_choices,
                function(x)
                  setNames(lapply(x$choices, "[[", "value"),
                           vapply(x$choices,
                                  function(cc) gsub(" / ", "/", tolower(cc$label)),
                                    character(1))))
param <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
.ridl_resource_lookup_list <- setNames(value, param)

lookup_rr_list_to_df <- function(key) {
  df <- tibble::tibble(code = unlist(.ridl_resource_lookup_list[[key]]),
                       label = names(.ridl_resource_lookup_list[[key]]))
  names(df) <- paste0(key, "_", names(df))
  readr::type_convert(df)
}

nm <- names(.ridl_resource_lookup_list)
all_df <- lapply(names(.ridl_resource_lookup_list), lookup_rr_list_to_df)
names(all_df) <- paste0("rr_metadata_", gsub("\\-", "", nm))
list2env(all_df, globalenv())

### Container
bool <- vapply(.ridl_container_schema$fields,
               function(x) "choices" %in% names(x),
               logical(1))

param_with_choices <- .ridl_container_schema$fields[bool]
value <- lapply(param_with_choices,
                function(x)
                  setNames(lapply(x$choices, "[[", "value"),
                           vapply(x$choices,
                                  function(cc) gsub(" / ", "/", tolower(cc$label)),
                                    character(1))))
param <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
.ridl_container_lookup_list <- setNames(value, param)

lookup_rc_list_to_df <- function(key) {
  df <- tibble::tibble(code = unlist(.ridl_container_lookup_list[[key]]),
                       label = names(.ridl_container_lookup_list[[key]]))
  names(df) <- paste0(key, "_", names(df))
  readr::type_convert(df)
}

nm <- names(.ridl_container_lookup_list)
all_df <- lapply(names(.ridl_container_lookup_list), lookup_rc_list_to_df)
names(all_df) <- paste0("rc_metadata_", nm)
list2env(all_df, globalenv())

usethis::use_data(.ridl_dataset_schema,
                  .ridl_deposited_dataset_schema,
                  .ridl_container_schema,
                  .ridl_dataset_lookup_list,
                  .ridl_resource_lookup_list,
                  .ridl_container_lookup_list,
                  internal = TRUE,
                  overwrite = TRUE)

usethis::use_data(rd_metadata_archived,
                  rd_metadata_data_collection_technique,
                  rd_metadata_data_sensitivity,
                  rd_metadata_external_access_level,
                  rd_metadata_identifiability,
                  rd_metadata_keywords,
                  rd_metadata_operational_purpose_of_data,
                  rd_metadata_process_status,
                  rd_metadata_sampling_procedure,
                  rd_metadata_visibility,
                  rr_metadata_confidentiality_status,
                  rr_metadata_file_type,
                  rr_metadata_hxlated,
                  rr_metadata_identifiability,
                  rr_metadata_process_status,
                  rr_metadata_type,
                  rr_metadata_visibility,
                  rc_metadata_country,
                  rc_metadata_sectoral_area,
                  overwrite = TRUE)
