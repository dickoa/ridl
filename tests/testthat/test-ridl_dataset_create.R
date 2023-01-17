test_that("ridl_dataset returns a RIDLDataset", {
  output <- ridl_dataset(title = "Motor Trend Car Road Tests",
                         name = "mtcars_rbwca",
                         notes = "The data was extracted from the 1974 _Motor Trend_ US magazine, and comprises fuel consumption and 10 aspects of automobile design",
                         owner_org = "west-africa",
                         visibility = "public",
                         geographies = "UNSPECIFIED",
                         external_access_level = "open_access",
                         data_collector = "Henderson and Velleman",
                         keywords = c("health", "protection"),
                         unit_of_measurement = "car",
                         data_collection_technique = "f2f",
                         archived = "False")

  expect_is(output, c("RIDLDataset", "RIDLObject", "R6"))
})
