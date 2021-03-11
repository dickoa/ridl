library(ridl)
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
usethis::use_data(.ridl_dataset_lookup_list, internal = TRUE)