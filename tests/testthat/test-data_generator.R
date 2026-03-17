test_that("generate_courses works correctly", {
  courses <- generate_courses(min_cap_range = c(5, 10))

  expect_s3_class(courses, "data.frame")
  expect_equal(nrow(courses), 40)
  expect_true(all(courses$min_capacity >= 5 & courses$min_capacity <= 10))
  expect_true(all(courses$max_capacity >= 7))
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
})

test_that("generate_students throws errors on invalid input", {
  courses_empty <- data.frame()
  expect_error(generate_students(10, courses_empty), "Es muessen mindestens 3 Kurse")
  
  # Bad data frame missing columns
  courses_bad <- data.frame(id = c("A", "B", "C"))
  expect_error(generate_students(10, courses_bad))
})
