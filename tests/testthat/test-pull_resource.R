context("resource")

test_that("Resource must returns the correct output", {
  skip_on_cran()
  skip_if_offline()
  set_ridl_config()
  expect_is(pull_resource("ce4c2e9b-d0ff-4adb-93ef-d8f553b9e6e1"), "Resource")
})
