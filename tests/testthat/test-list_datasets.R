context("list_ridl_dataset")

test_that("list_ridl_dataset returns a vector of character", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  vcr::use_cassette("list_ridl_dataset", {
    output <- list_ridl_dataset()
  })
  expect_is(output, "character")
})
