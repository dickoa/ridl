test_that("create_ridl_config must returns a Configuration object", {
  expect_is(ridl_config(), "RIDLConfig")
})

test_that("API key must be valid i.e uuid", {
  skip_on_cran()
  expect_error(ridl_config_set(key = "abcdefd"))
})
