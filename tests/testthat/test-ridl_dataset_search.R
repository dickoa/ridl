context("ridl_dataset_search")

test_that("search_datasets returns the correct output", {
  skip_on_cran()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_search", {
    output <- ridl_dataset_search(rows = 5L)
  })
  expect_is(output,
            "ridl_dataset_list")
})

test_that("ridl_dataset_search can return only public datasets", {
  skip_on_cran()
  ridl_config_setup()
  vcr::use_cassette("ridl_dataset_search_visibility", {
    output <- ridl_dataset_search(visibility = "public",
                                  rows = 10L)
  })
  visibility <- vapply(output, function(ds) ds$data$visibility,
                       character(1))
  expect_length(visibility, 10L)
  expect_equal(unique(visibility),
               "public")
})
