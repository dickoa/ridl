context("pull_ridl_dataset")

test_that("pull_ridl_dataset must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_ridl_dataset", {
    output <- pull_ridl_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(output, "RIDLDataset")
})

test_that("get_ridl_resource must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_ridl_dataset", {
    output <- pull_ridl_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_ridl_resource(output, 1), "RIDLResource")
})

test_that("list_ridl_resource must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_ridl_dataset", {
    output <- pull_ridl_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(list_ridl_resource(output), "ridl_resource_list")
  expect_is(list_ridl_resource(output)[[1]], "RIDLResource")
  expect_that(length(list_ridl_resource(delete_all_ridl_resource(output))),
              equals(0))
})

test_that("get_ridl_container allows to get the container of the dataset", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("pull_ridl_dataset", {
    output <- pull_ridl_dataset("ddi-uga-unhcr-kap-2019-kyangwali-v1-0")
  })
  expect_is(get_ridl_container(output), "RIDLContainer")
})
