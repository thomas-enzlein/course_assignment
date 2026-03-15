test_that("generate_courses works correctly", {
  courses <- generate_courses(min_cap = 5, max_cap = 25)

  expect_s3_class(courses, "data.frame")
  expect_equal(nrow(courses), 50)
  expect_equal(courses$min_capacity[1], 5)
  expect_equal(courses$max_capacity[1], 25)
  expect_true(all(courses$weight_m > 0))
  expect_true(all(courses$weight_w > 0))
})

test_that("generate_students works correctly", {
  courses <- generate_courses()
  students <- generate_students(n = 50, courses = courses)

  expect_s3_class(students, "data.frame")
  expect_equal(nrow(students), 50)
  expect_true(all(students$gender %in% c("m", "w")))

  # Check if 3 distinct choices were made
  for (i in seq_len(nrow(students))) {
    choices <- c(students$first_choice[i], students$second_choice[i], students$third_choice[i])
    expect_equal(length(unique(choices)), 3)
    expect_true(all(choices %in% courses$course_id))
  }

  # Check tie-breaker range
  expect_true(all(students$tie_breaker >= 0.001 & students$tie_breaker <= 0.099))
})
test_that("generate_students throws errors on invalid input", {
  courses_empty <- data.frame()
  expect_error(generate_students(10, courses_empty), "Es muessen mindestens 3 Kurse")
  
  courses_bad <- data.frame(course_id = c("A", "B", "C"), x = 1, y = 2)
  expect_error(generate_students(10, courses_bad))
})
