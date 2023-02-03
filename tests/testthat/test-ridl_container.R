test_that("ridl_container can be created using R6 and S3", {

  ct <- RIDLContainer$new(list(country = "MLI",
                               visible_external = "False",
                               geographic_area = "Mali"))

  ct2 <- ridl_container(country = "MLI",
                        visible_external = FALSE,
                        geographic_area = "Mali")

  expect_equal(ct, ct2)
  expect_is(ct, c("RIDLContainer", "RIDLObject", "R6"))
})
