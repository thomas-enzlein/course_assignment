test_that("example_data_path returns a valid path", {
  # Check if we can get the path
  path <- example_data_path()
  
  expect_type(path, "character")
  expect_true(nchar(path) > 0)
  expect_true(file.exists(path))
  expect_true(grepl("\\.xlsx$", path))
})
