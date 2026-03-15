test_that("export_results generates files and console output", {
  skip_if_not_installed("utils")
  
  # Create a temporary directory
  tmp_dir <- tempdir()
  
  # Setup mock data for assignments
  assignments <- data.frame(
    student_id = c("1", "2"),
    course_id = c("A", "C"),
    stringsAsFactors = FALSE
  )
  
  res_mock <- list(
    assignments = assignments,
    status = "optimal",
    objective_value = 50
  )
  
  s_data <- data.frame(
    student_id = c("1", "2"),
    student_name = c("Anna", "Bob"),
    first_choice = c("A", "B"),
    second_choice = c("B", "C"),
    third_choice = c("C", "A"),
    stringsAsFactors = FALSE
  )
  
  c_data <- data.frame(
    course_id = c("A", "B", "C"),
    course_name = c("Mathe", "Bio", "Sport"),
    min_capacity = c(1, 1, 1),
    max_capacity = c(2, 2, 2),
    stringsAsFactors = FALSE
  )
  
  # The output is messages to the console
  out_msg <- capture_messages({
    export_results(res_mock, c_data, s_data, tmp_dir)
  })
  
  expect_true(any(grepl("Erfolgreich exportiert nach", out_msg)))
  
  # Check if files exist
  expect_true(file.exists(file.path(tmp_dir, "zuweisungen_schueler.csv")))
  expect_true(file.exists(file.path(tmp_dir, "kurs_zusammenfassung.csv")))
  
  # Read files back and check contents
  assign_df <- read.csv(file.path(tmp_dir, "zuweisungen_schueler.csv"), stringsAsFactors = FALSE)
  stat_df <- read.csv(file.path(tmp_dir, "kurs_zusammenfassung.csv"), stringsAsFactors = FALSE)
  
  expect_equal(nrow(assign_df), 2)
  expect_equal(nrow(stat_df), 3)
})

test_that("export_results handles failure state gracefully", {
  res_mock_fail <- list(
    assignments = data.frame(student_id = character(), course_id = character()),
    status = "infeasible"
  )
  
  s_data_empty <- data.frame(student_id = character(), first_choice = character(), second_choice = character(), third_choice = character())
  c_data_empty <- data.frame(course_id = character())
  
  expect_error(
    export_results(res_mock_fail, c_data_empty, s_data_empty, tempdir()),
    "Keine validen Ergebnisse zum Exportieren vorhanden"
  )
})
