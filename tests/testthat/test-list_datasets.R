context("list_datasets")

test_that("list_datasets returns a vector of character", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("list_datasets", {
    output <- list_datasets()
  })
  expect_is(output, "character")
})
