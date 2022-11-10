library("vcr")

vcr_dir <- "../fixtures"

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
  preserve_exact_body_bytes = TRUE,
  filter_request_headers = list(Authorization = "My bearer token is safe")
))
vcr::check_cassette_names()
