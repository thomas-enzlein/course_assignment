test_that("evaluate_dashboard works correctly", {
  # Mock result from optimize_courses
  
  students <- data.frame(
    student_id = c("1", "2", "3", "4"),
    student_name = c("A", "B", "C", "D"),
    first_choice = c("C1", "C2", "C1", "C3"),
    second_choice = c("C2", "C1", "C3", "C1"),
    third_choice = c("C3", "C3", "C2", "C2"),
    tie_breaker = c(0.01, 0.02, 0.03, 0.04),
    stringsAsFactors = FALSE
  )
  
  courses <- data.frame(
    course_id = c("C1", "C2", "C3"),
    course_name = c("Course 1", "Course 2", "Course 3"),
    min_capacity = c(1, 1, 1),
    max_capacity = c(2, 2, 2),
    stringsAsFactors = FALSE
  )
  
  assignments <- data.frame(
    student_id = c("1", "2", "3"),
    course_id = c("C1", "C2", "C3"),
    stringsAsFactors = FALSE
  )
  
  res_mock <- list(
    assignments = assignments,
    status = "optimal",
    objective_value = 100
  )
  
  # evaluate_dashboard produces console output, we can capture it
  out <- capture.output({
    evaluate_dashboard(res_mock, students, courses)
  })
  
  expect_true(any(grepl("KURSWAHL AUSWERTUNG", out)))
  expect_true(any(grepl("Solver Status: optimal", out)))
  expect_true(any(grepl("1. Wahl erhalten", out)))
  
  # Test with timeout status
  res_mock_timeout <- res_mock
  res_mock_timeout$status <- "error"
  
  out <- capture.output({
    evaluate_dashboard(res_mock_timeout, students, courses)
  })
  expect_true(any(grepl("Abbruch durch Zeitlimit", out)))
  
  # Test with someone getting 3rd choice
  assignments_3rd <- data.frame(
    student_id = c("1", "4"),
    course_id = c("C1", "C2"),
    stringsAsFactors = FALSE
  )
  res_3rd <- list(assignments = assignments_3rd, status = "optimal", objective_value = 80)
  out <- capture.output({
    evaluate_dashboard(res_3rd, students, courses)
  })
  expect_true(any(grepl("3. Wahl erhalten", out)))

  # Test with "Perfekt!" status (everyone assigned)
  students_2 <- students[1:2, ]
  assignments_2 <- data.frame(student_id = c("1", "2"), course_id = c("C1", "C2"), stringsAsFactors = FALSE)
  res_perfekt <- list(assignments = assignments_2, status = "optimal", objective_value = 100)
  out <- capture.output({
    evaluate_dashboard(res_perfekt, students_2, courses)
  })
  expect_true(any(grepl("Perfekt!", out)))
  
  # Test with > 10 unassigned
  # Generate 15 students
  students_many <- data.frame(
    student_id = sprintf("%d", 1:15),
    student_name = "test",
    first_choice = "C1", second_choice = "C2", third_choice = "C3",
    tie_breaker = 0.01,
    stringsAsFactors = FALSE
  )
  res_empty <- list(assignments = data.frame(student_id=character(), course_id=character()), status = "optimal", objective_value = 0)
  out <- capture.output({
    evaluate_dashboard(res_many <- list(assignments = data.frame(student_id=character(), course_id=character()), status="optimal", objective_value=0), students_many, courses)
  })
  expect_true(any(grepl("... und weitere.", out)))
})
