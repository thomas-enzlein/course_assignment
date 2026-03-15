test_that("example_data_path returns correct paths", {
  # These files should exist if the package is built or if we test interactively
  # For devtools::test or R CMD check they are in inst/extdata
  
  # Check if we can get paths
  path_stud <- example_data_path("students")
  path_course <- example_data_path("courses")
  
  expect_type(path_stud, "character")
  expect_type(path_course, "character")
  
  expect_true(nchar(path_stud) > 0)
  expect_true(nchar(path_course) > 0)
  
  # Error on invalid argument
  expect_error(example_data_path("invalid"), "should be one of")
})
