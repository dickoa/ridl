test_that("ridl_resource returns a RIDLResource", {
  output <- ridl_resource(type = "data",
                          date_range_start = Sys.Date() - 10,
                          date_range_end = Sys.Date(),
                          file_type = "microdata",
                          hxlated = FALSE,
                          process_status = "raw",
                          identifiability = "personally_identifiable",
                          visibility = "public",
                          measurement_unit = "persons",
                          methodology = "collected using face to face",
                          name = "my.csv",
                          version = as.character(Sys.Date()),
                          format = "csv",
                          description = "A cool csv")

  expect_is(output, c("RIDLResource", "RIDLObject", "R6"))
})
