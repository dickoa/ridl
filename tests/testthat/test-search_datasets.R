context("search_datasets")

test_that("search_datasets returns the correct output", {
  skip_on_cran()
  set_ridl_config()
  expect_is(search_ridl_dataset(),
            "ridl_dataset_list")
})

test_that("search_datasets can return only public datasets", {
  skip_on_cran()
  set_ridl_config()
  vcr::use_cassette("search_ridl_dataset", {
    output <- search_ridl_dataset(visibility = "public",
                                  rows = 10L)
  })
  visibility <- vapply(output, function(ds) ds$data$visibility,
                       character(1))
  expect_length(visibility, 10L)
  expect_equal(unique(visibility),
               "public")
})
