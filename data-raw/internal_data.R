library(jsonlite)

## URL
dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/dataset.json"
data_container_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/data_container.json"
deposited_dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/deposited_dataset.json"

.ridl_dataset_schema <- fromJSON(dataset_json,
                                  simplifyVector = FALSE)
.ridl_deposited_dataset_schema <- fromJSON(deposited_dataset_json,
                                           simplifyVector = FALSE)
.ridl_container_schema <<- fromJSON(data_container_json,
                                    simplifyVector = FALSE)

bool <- vapply(.ridl_dataset_schema$dataset_fields,
               function(x) "choices" %in% names(x),
               logical(1))

param_with_choices <- .ridl_dataset_schema$dataset_fields[bool]
value <- lapply(param_with_choices,
                function(x)
                  setNames(lapply(x$choices, "[[", "value"),
                           vapply(x$choices,
                                  function(cc) tolower(cc$label),
                                    character(1))))
param <- vapply(param_with_choices,
                function(x) x$field_name, character(1))
.ridl_dataset_lookup_list <- setNames(value, param)

usethis::use_data(.ridl_dataset_schema,
                  .ridl_deposited_dataset_schema,
                  .ridl_container_schema,
                  .ridl_dataset_lookup_list,
                  internal = TRUE,
                  overwrite = TRUE)
