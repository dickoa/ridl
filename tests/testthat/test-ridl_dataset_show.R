context("ridl_dataset_show")

test_that("ridl_dataset_show must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_type", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(output, "RIDLDataset")
})

test_that("ridl_resource_get must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_resource_get", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(ridl_resource_get(output, 1), "RIDLResource")
})

test_that("ridl_resource_list must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_resource_list", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(ridl_resource_list(output), "ridl_resource_list")
  expect_is(ridl_resource_list(output)[[1]], "RIDLResource")
  expect_that(length(ridl_resource_list(ridl_resource_delete_all(output))),
              equals(0))
})

test_that("ridl_container_get allows to get the container of the dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_container_get", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(ridl_container_get(output), "RIDLContainer")
})
