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
#' @param time_limit_sec Zeitlimit fuer den Solver in Sekunden. Standard ist 15.
#' @return Ein Listen-Objekt mit den Zuweisungen und Status
#' @export
#' @import ompr
#' @import ompr.roi
#' @import ROI.plugin.glpk
optimize_courses <- function(students, courses, enforce_survival = FALSE, time_limit_sec = 15) {
  n_students <- nrow(students)
  n_courses <- nrow(courses)

  # Hilfsmatrix fuer den Score aufbauen (-100 verhindert Zuweisung an un-gewaehlte Kurse)
  score_matrix <- matrix(-100, nrow = n_students, ncol = n_courses)

  # Scores befuellen
  for (i in 1:n_students) {
    # HIER: Schutz vor Mehrfachwahl desselben Kurses
    # Wir nehmen nur die jeweils höchste Priorität
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

  # Zielfunktions-Ausdruck (als Funktion, um Variablen auszuwerten)
  value_func <- function(i, j) get_score(score_matrix, i, j)

  # Basis Modell
  model <- MIPModel() |>
    add_variable(x[i, j], i = 1:n_students, j = 1:n_courses, type = "binary") |>
    add_variable(y[j], j = 1:n_courses, type = "binary") |>

    # Jeder Schueler bekommt maximal 1 Kurs (<= 1 erlaubt "Reste-Rampe")
    add_constraint(sum_expr(x[i, j], j = 1:n_courses) <= 1, i = 1:n_students) |>

    # Kapazitaet nach oben
    add_constraint(sum_expr(x[i, j], i = 1:n_students) <= max_cap[j] * y[j], j = 1:n_courses) |>

    # Kapazitaet nach unten
    add_constraint(sum_expr(x[i, j], i = 1:n_students) >= min_cap[j] * y[j], j = 1:n_courses) |>

    # STRENG: Keine Zuweisung an Kurse, die nicht gewaehlt wurden
    add_constraint(x[i, j] == 0, i = 1:n_students, j = 1:n_courses, score_matrix[i, j] == -100)

  # Je nach Flag die Zielfunktion setzen
  if (enforce_survival) {
    model <- model |>
      set_objective(
        sum_expr(value_func(i, j) * x[i, j], i = 1:n_students, j = 1:n_courses) +
          sum_expr(20 * y[j], j = 1:n_courses),
        "max"
      )
  } else {
    model <- model |>
      set_objective(
        sum_expr(value_func(i, j) * x[i, j], i = 1:n_students, j = 1:n_courses),
        "max"
      )
  }

  # Modell loesen mit GLPK (Zeitlimit, da das Problem kombinatorisch extrem komplex ist)
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
