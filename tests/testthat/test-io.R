test_that("import_data checks required files and structure", {
  # Temporäre Excel-Datei erzeugen
  dir_path <- tempdir()
  excel_file <- file.path(dir_path, "test_data.xlsx")

  # Gültige Kurse
  c_data <- data.frame(course_id = c("A", "B", "C"), course_name = c("n1", "n2", "n3"), min_capacity = c(2, 2, 2), max_capacity = c(5, 5, 5), stringsAsFactors = FALSE)
  # Gültige Schüler (ohne tie_breaker)
  s_data <- data.frame(student_id = c("1", "2"), student_name = c("n1", "n2"), first_choice = c("A", "B"), second_choice = c("B", "C"), third_choice = c("C", "A"), stringsAsFactors = FALSE)

  writexl::write_xlsx(list("Kurse" = c_data, "Schüler" = s_data), excel_file)

  # Valid import
  res <- import_data(excel_file)
  expect_equal(nrow(res$courses), 3)
  expect_equal(nrow(res$students), 2)
  expect_true("tie_breaker" %in% names(res$students)) # wurde auto-generiert

  # Defekter Schüler (Wahl existiert nicht)
  s_data_bad <- s_data
  s_data_bad$first_choice[1] <- "D"
  writexl::write_xlsx(list("Kurse" = c_data, "Schüler" = s_data_bad), excel_file)

  expect_error(import_data(excel_file), "existieren")

  # Cleanup
  unlink(excel_file)
})

test_that("export_results works", {
  c_data <- data.frame(course_id = c("A", "B", "C"), course_name = c("A_N", "B_N", "C_N"), min_capacity = c(2, 2, 2), max_capacity = c(5, 5, 5), stringsAsFactors = FALSE)

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

test_that("import_data handles missing files", {
  expect_error(import_data("nonexistent.xlsx"), "Datei.*konnte nicht gefunden werden")
})

test_that("import_data handles validation errors", {
  # Create some temp files with bad data
  excel_bad <- tempfile(fileext = ".xlsx")
  
  c_bad <- data.frame(bad_col = 1, course_name = "n")
  s_ok <- data.frame(student_id = "1", student_name = "n", first_choice = "A", second_choice = "A", third_choice = "A")
  
  writexl::write_xlsx(list("Kurse" = c_bad, "Schüler" = s_ok), excel_bad)
  expect_error(import_data(excel_bad), "Es fehlen zwingend benoetigte Spalten")
  
  # Duplicated course_id
  c_dup <- data.frame(course_id = c("A", "A"), min_capacity = 1, max_capacity = 2)
  writexl::write_xlsx(list("Kurse" = c_dup, "Schüler" = s_ok), excel_bad)
  expect_error(import_data(excel_bad), "Die 'course_id' muss absolut eindeutig sein")
  
  # Cleanup
  unlink(excel_bad)
})

test_that("update_course_capacity validates and updates correctly", {
  c_data <- data.frame(
    course_id = c("A", "B"), 
    min_capacity = c(2, 2), 
    max_capacity = c(5, 5), 
    stringsAsFactors = FALSE
  )
  
  # Valid update
  res <- update_course_capacity(c_data, 1, "min_capacity", 10)
  expect_equal(res$min_capacity[1], 10)
  
  # Invalid row
  expect_error(update_course_capacity(c_data, 3, "min_capacity", 10), "Zeilenindex")
  
  # Invalid column
  expect_error(update_course_capacity(c_data, 1, "course_id", 10), "Spaltenname")
  
  # Invalid value
  expect_error(update_course_capacity(c_data, 1, "min_capacity", -1), "Ganzzahl")
  expect_error(update_course_capacity(c_data, 1, "min_capacity", "abc"), "Ganzzahl")
})
