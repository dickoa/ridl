context("ridl_dataset_list")

test_that("ridl_dataset_list returns a vector of character", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_list", {
    output <- ridl_dataset_list()
  })
  expect_is(output, "character")
})
