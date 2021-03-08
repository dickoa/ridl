context("ridl_resource")

test_that("ridl_resource returns a RIDLResource", {
  data <- list(type = "data",
               date_range_start = Sys.Date() - 10,
               date_range_end = Sys.Date(),
               file_type = "report",
               `hxl-ated` = "False",
               process_status = "raw",
               identifiability = "personally_identifiable",
               name = "my.csv",
               title = "My CSV",
               version = Sys.Date(),
               format = "csv",
               description = "A cool csv")

  skip_on_cran()
  skip_if_offline()
  expect_is(ridl_resource(data), "RIDLResource")
})
