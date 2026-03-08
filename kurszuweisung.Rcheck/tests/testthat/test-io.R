test_that("import_data checks required files and structure", {
  # Temporäre Dateien erzeugen
  dir_path <- tempdir()
  c_file <- file.path(dir_path, "c_test.csv")
  s_file <- file.path(dir_path, "s_test.csv")
  
  # Gültige Kurse
  c_data <- data.frame(course_id = c("A", "B", "C"), min_capacity = c(2,2,2), max_capacity = c(5,5,5), stringsAsFactors = FALSE)
  write.csv(c_data, c_file, row.names = FALSE)
  
  # Gültige Schüler (ohne tie_breaker)
  s_data <- data.frame(student_id = c("1", "2"), first_choice = c("A", "B"), second_choice = c("B", "C"), third_choice = c("C", "A"), stringsAsFactors = FALSE)
  write.csv(s_data, s_file, row.names = FALSE)
  
  # Valid import
  res <- import_data(s_file, c_file)
  expect_equal(nrow(res$courses), 3)
  expect_equal(nrow(res$students), 2)
  expect_true("tie_breaker" %in% names(res$students)) # wurde auto-generiert
  
  # Defekter Schüler (Wahl existiert nicht)
  s_data_bad <- s_data
  s_data_bad$first_choice[1] <- "D"
  write.csv(s_data_bad, s_file, row.names = FALSE)
  
  expect_error(import_data(s_file, c_file), "existieren")
  
  # Cleanup
  unlink(c_file)
  unlink(s_file)
})

test_that("export_results works", {
  c_data <- data.frame(course_id = c("A", "B", "C"), course_name = c("A_N", "B_N", "C_N"), min_capacity = c(2,2,2), max_capacity = c(5,5,5), stringsAsFactors = FALSE)
  
  assign_data <- data.frame(student_id = c("1", "2"), course_id = c("A", "A"), choice_num = c(1, 2), stringsAsFactors = FALSE)
  
  dummy_result <- list(status = "success", assignments = assign_data)
  s_data <- data.frame(student_id = c("1", "2"), first_choice = c("A", "B"), second_choice = c("B", "A"), third_choice = c("C", "C"), stringsAsFactors = FALSE)
  
  out_dir <- file.path(tempdir(), "test_export")
  export_results(dummy_result, c_data, s_data, out_dir)
  
  expect_true(file.exists(file.path(out_dir, "zuweisungen_schueler.csv")))
  expect_true(file.exists(file.path(out_dir, "kurs_zusammenfassung.csv")))
  
  # Cleanup
  unlink(out_dir, recursive = TRUE)
})
