context("pull_dataset")

test_that("pull_dataset must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(output, "RIDLDataset")
})

test_that("get_resource must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_dataset_nth_resource(output, 1), "RIDLResource")
})

test_that("list_dataset_resources must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(list_dataset_resources(output), "ridl_resources_list")
  expect_is(list_dataset_resources(output)[[1]], "RIDLResource")
  expect_that(length(list_dataset_resources(delete_dataset_resources(output))),
              equals(0))
})

test_that("get_dataset_container allows to get the container of the dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_dataset_container(output), "RIDLContainer")
})

test_that("get_dataset_resources_formats gives dataset resources formats", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_dataset_resources_formats(output), "character")
})
