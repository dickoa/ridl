context("list_datasets")

test_that("list_datasets returns a vector of character", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("list_datasets", {
    output <- list_datasets(limit = 10, offset = 5)
  })
  expect_is(output, "character")
  expect_length(output, 10L)
})
