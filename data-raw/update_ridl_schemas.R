## URL
dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/dataset.json"
data_container_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/data_container.json"
deposited_dataset_json <- "https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/deposited_dataset.json"


download.file(dataset_json,
              destfile = "../inst/schemas/dataset.json")
download.file(data_container_json,
              destfile = "../inst/schemas/data_container.json")
download.file(deposited_dataset_json,
              destfile = "../inst/schemas/deposited_dataset.json")
