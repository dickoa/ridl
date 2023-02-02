test_that("ridl_resource returns a RIDLContainer", {
  output <- ridl_container(country = "MLI",
                           title = "Mali",
                           name = "mli",
                           description = "A container on Mali",
                           visible_external = FALSE,
                           geographic_area = "Mali")

  expect_is(output, c("RIDLContainer", "RIDLObject", "R6"))
})
