context("set_ridl_config")

test_that("create_ridl_config must returns a Configuration object", {
  expect_is(ridl_config(), "RIDLConfig")
})

test_that("API key must be valid i.e uuid", {
  skip_on_cran()
  expect_error(set_ridl_config(ridl_key = "abcdefd"))
})
