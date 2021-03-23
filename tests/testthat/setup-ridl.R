library("vcr")
invisible(vcr::vcr_configure(
  filter_sensitive_data = list("<<<my_api_key>>>" = Sys.getenv('RIDL_API_KEY')),  # add this
  dir = "../fixtures"
))
vcr::check_cassette_names()