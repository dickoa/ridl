test_that("ridl_dataset_list returns a vector of character", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_list", {
    output <- ridl_dataset_list()
  })
  expect_type(output, "character")
})

test_that("ridl_dataset_list can be use to list container datasets", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_list_container", {
    cont <- ridl_container_show("ethiopia-sens")
    output <- ridl_dataset_list(container = cont)
  })
  expect_type(output, "character")
})
