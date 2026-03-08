test_that("optimize_courses works", {
  # Mini Setup
  courses <- data.frame(
    course_id = c("C1", "C2", "C3"),
    course_name = c("Kurs 1", "Kurs 2", "Kurs 3"),
    min_capacity = c(2, 2, 2),
    max_capacity = c(5, 5, 5),
    stringsAsFactors = FALSE
  )
  
  students <- data.frame(
    student_id = c("S1", "S2", "S3", "S4", "S5"),
    first_choice = c("C1", "C1", "C2", "C3", "C2"),
    second_choice = c("C2", "C3", "C1", "C1", "C3"),
    third_choice = c("C3", "C2", "C3", "C2", "C1"),
    tie_breaker = runif(5, 0.001, 0.099),
    stringsAsFactors = FALSE
  )
  
  res <- optimize_courses(students, courses, enforce_survival = FALSE)
  
  expect_true(res$status %in% c("optimal", "success"))
  expect_true(nrow(res$assignments) <= 5)
})
