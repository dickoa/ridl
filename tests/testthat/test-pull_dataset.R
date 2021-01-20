context("dataset")

test_that("pull_dataset must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(output, "Dataset")
})

test_that("get_resource must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_dataset_resource(output, 1), "Resource")
})

test_that("get_resources must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(list_dataset_resources(output), "resources_list")
  expect_is(list_dataset_resources(output)[[1]], "Resource")
  expect_that(length(list_dataset_resources(delete_all_dataset_resources(output))),
              equals(0))
})
