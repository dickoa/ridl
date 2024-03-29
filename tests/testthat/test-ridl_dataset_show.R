test_that("ridl_dataset_show must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_type", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(output,
            c("RIDLDataset", "RIDLObject", "R6"))
})

test_that("ridl_dataset_resource_get must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_resource_get", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(ridl_dataset_resource_get(output, 1),
            c("RIDLResource", "RIDLObject", "R6"))
})

test_that("ridl_dataset_resource_get_all must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_resource_get_all", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(rd_resource_get_all(output), "ridl_resource_list")
  expect_is(rd_resource_get_all(output)[[1]],
            c("RIDLResource", "RIDLObject", "R6"))
  expect_that(length(rd_resource_get_all(rd_resource_delete_all(output))),
              equals(0))
})

test_that("ridl_dataset_container_get allows to get the container of the dataset", {
  skip_on_cran()
  skip_if_offline()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_container_get", {
    output <- ridl_dataset_show("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(rd_container_get(output),
            c("RIDLContainer", "RIDLObject", "R6"))
})
