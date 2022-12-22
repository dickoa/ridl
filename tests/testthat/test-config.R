test_that("create_ridl_config must returns a Configuration object", {
  expect_is(ridl_config(), "RIDLConfig")
})
