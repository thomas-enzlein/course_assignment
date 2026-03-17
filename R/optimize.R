#' Gibt den Matrixwert fuer die Score Matrix zurueck
#' @keywords internal
get_score <- function(score_matrix, i, j) {
  vapply(seq_along(i), function(k) score_matrix[i[k], j[k]], numeric(1L))
}

#' Kurswahl optimieren
#'
#' Berechnet die optimale Zuweisung basierend auf Schueler-Wünschen und Kurs-Kapazitäten.
#' Schützt automatisch vor Mehrfachwahlen desselben Kurses durch einen Schueler.
#'
#' @param students data.frame mit Schueler-Praeferenzen
#' @param courses data.frame mit Kurs-Kapazitaeten
#' @param enforce_survival Logisch, ob Kurse bevorzugt gerettet werden sollen. Standard ist FALSE.
#' @param balance_gender Numerisch (0-100), Staerke der Geschlechter-Gleichverteilung. Standard ist 0.
#' @param balance_class Numerisch (0-100), Staerke der Klassen-Durchmischung. Standard ist 0.
#' @param time_limit_sec Zeitlimit fuer den Solver in Sekunden. Standard ist 15.
#' @return Ein Listen-Objekt mit den Zuweisungen und Status
#' @export
#' @import ompr
#' @import ompr.roi
#' @import ROI.plugin.glpk
optimize_courses <- function(students, courses, enforce_survival = FALSE, 
                             balance_gender = 0, balance_class = 0,
                             time_limit_sec = 15) {
  n_students <- nrow(students)
  n_courses <- nrow(courses)

  # Robustness: Ensure columns exist (e.g. if called directly in tests)
  if (!"gender" %in% names(students)) students$gender <- "k.A."
  if (!"class" %in% names(students)) students$class <- "k.A."
  if (!"tie_breaker" %in% names(students)) students$tie_breaker <- runif(n_students, 0.001, 0.099)

  # Hilfsmatrix fuer den Score aufbauen (-100 verhindert Zuweisung an un-gewaehlte Kurse)
  score_matrix <- matrix(-100, nrow = n_students, ncol = n_courses)

  # Scores befuellen
  for (i in 1:n_students) {
    # HIER: Schutz vor Mehrfachwahl desselben Kurses
    c1 <- students$first_choice[i]
    c2 <- if (students$second_choice[i] != c1) students$second_choice[i] else ""
    c3 <- if (students$third_choice[i] != c1 && students$third_choice[i] != c2) students$third_choice[i] else ""

    c1_idx <- which(courses$course_id == c1)
    c2_idx <- which(courses$course_id == c2)
    c3_idx <- if (c3 != "") which(courses$course_id == c3) else integer(0)

    if (length(c1_idx) == 1) score_matrix[i, c1_idx] <- 5 + students$tie_breaker[i]
    if (length(c2_idx) == 1) score_matrix[i, c2_idx] <- 3 + students$tie_breaker[i]
    if (length(c3_idx) == 1) score_matrix[i, c3_idx] <- 1 + students$tie_breaker[i]
  }
  min_cap <- courses$min_capacity
  max_cap <- courses$max_capacity

  # Check Data Quality for Balancing
  has_gender_data <- any(students$gender != "k.A.")
  has_class_data <- any(students$class != "k.A.")
  
  if (balance_gender > 0 && !has_gender_data) {
    warning("Geschlechter-Balance angefordert, aber keine Geschlechts-Daten vorhanden. Regel wird ignoriert.")
    balance_gender <- 0
  }
  
  if (balance_class > 0 && !has_class_data) {
    warning("Klassen-Mix angefordert, aber keine Klassen-Daten vorhanden. Regel wird ignoriert.")
    balance_class <- 0
  }

  # Gender-Mapping (fuer die Constraints)
  is_boy <- if (has_gender_data) as.numeric(students$gender == "m") else rep(0, n_students)
  global_boy_ratio <- if (has_gender_data) mean(is_boy, na.rm = TRUE) else 0.5
  if (is.na(global_boy_ratio)) global_boy_ratio <- 0.5

  # Klassen-Mapping (fuer Durchmischung)
  all_classes <- if (has_class_data) unique(students$class) else "k.A."
  n_classes <- length(all_classes)
  class_membership <- matrix(0, nrow = n_students, ncol = n_classes)
  for (k in seq_along(all_classes)) {
    class_membership[, k] <- as.numeric(students$class == all_classes[k])
  }

  # Zielfunktions-Ausdruck
  value_func <- function(i, j) get_score(score_matrix, i, j)

  # Basis Modell
  model <- MIPModel() |>
    add_variable(x[i, j], i = 1:n_students, j = 1:n_courses, type = "binary") |>
    add_variable(y[j], j = 1:n_courses, type = "binary") |>
    add_variable(z_gender[j], j = 1:n_courses, type = "continuous", lb = 0) |>
    add_variable(z_class[j, k], j = 1:n_courses, k = 1:n_classes, type = "continuous", lb = 0) |>
    
    # Jeder Schueler bekommt maximal 1 Kurs
    add_constraint(sum_expr(x[i, j], j = 1:n_courses) <= 1, i = 1:n_students) |>
    # Kapazitaet nach oben
    add_constraint(sum_expr(x[i, j], i = 1:n_students) <= max_cap[j] * y[j], j = 1:n_courses) |>
    # Kapazitaet nach unten
    add_constraint(sum_expr(x[i, j], i = 1:n_students) >= min_cap[j] * y[j], j = 1:n_courses) |>
    # Keine Zuweisung an ungueltige Kurse
    add_constraint(x[i, j] == 0, i = 1:n_students, j = 1:n_courses, score_matrix[i, j] == -100)

  # --- Geschlechter-Balance (Soft Constraint via Penalty) ---
  if (balance_gender > 0) {
    model <- model |>
      # Penalty: |Boys_j - Ratio * Total_j| <= z_gender_j
      add_constraint(sum_expr(is_boy[i] * x[i, j], i = 1:n_students) - 
                       global_boy_ratio * sum_expr(x[i, j], i = 1:n_students) <= z_gender[j], j = 1:n_courses) |>
      add_constraint(global_boy_ratio * sum_expr(x[i, j], i = 1:n_students) - 
                       sum_expr(is_boy[i] * x[i, j], i = 1:n_students) <= z_gender[j], j = 1:n_courses)
  }

  # --- Klassen-Durchmischung (Soft Constraint) ---
  if (balance_class > 0 && n_classes > 1) {
    model <- model |>
      add_constraint(sum_expr(class_membership[i, k] * x[i, j], i = 1:n_students) - 
                       (1/n_classes) * sum_expr(x[i, j], i = 1:n_students) <= z_class[j, k], j = 1:n_courses, k = 1:n_classes)
  }

  # Zielfunktion zusammenstellen
  # Wir nutzen !! um externe Variablen in sum_expr zu injecten
  bg_weight <- balance_gender / 10
  bc_weight <- balance_class / 20
  surv_bonus <- if (enforce_survival) 20 else 0

  model <- model |> set_objective(
    sum_expr(value_func(i, j) * x[i, j], i = 1:n_students, j = 1:n_courses) +
      sum_expr(surv_bonus * y[j], j = 1:n_courses) -
      sum_expr(bg_weight * z_gender[j], j = 1:n_courses) -
      sum_expr(bc_weight * z_class[j, k], j = 1:n_courses, k = 1:n_classes),
    "max"
  )

  # Modell loesen
  result <- solve_model(model, with_ROI(solver = "glpk", control = list(tm_limit = time_limit_sec * 1000)))

  # Ergebnisse extrahieren
  optimal_x <- get_solution(result, x[i, j])
  optimal_y <- get_solution(result, y[j])

  assignments <- optimal_x[optimal_x$value == 1, ]
  assignments$student_id <- students$student_id[assignments$i]
  assignments$course_id <- courses$course_id[assignments$j]

  list(
    status = result$status,
    objective_value = result$objective_value,
    assignments = assignments,
    active_courses = optimal_y[optimal_y$value == 1, ],
    score_matrix = score_matrix
  )
}

# Globale Variablen fuer ompr (um R CMD check zu beruhigen)
if (getRversion() >= "2.15.1") utils::globalVariables(c("x", "y", "i", "j"))
