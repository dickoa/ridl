library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")

if (!nzchar(Sys.getenv("RIDL_API_KEY"))) {
  if (dir.exists(vcr_dir)) {
    # Fake API token to fool our package
    Sys.setenv("RIDL_API_KEY" = "foobar")
  } else {
    # If there's no mock files nor API token, impossible to run tests
    stop("No API key nor cassettes, tests cannot be run.",
         call. = FALSE)
  }
}

invisible(vcr::vcr_configure(
  dir = vcr_dir,
  # Filter the request header where the token is sent, make sure you know
  # how authentication works in your case and read the Security chapter :-)
  filter_request_headers = list(`X-CKAN-API-Key` = "safe_key")
))
vcr::check_cassette_names()
