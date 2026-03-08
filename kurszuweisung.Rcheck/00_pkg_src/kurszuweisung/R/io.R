#' Daten importieren und validieren
#'
#' Liest Schueler- und Kursdaten aus CSV-Dateien ein und validiert die Struktur rigoros.
#' 
#' @param students_file Pfad zur CSV-Datei mit Schuelerdaten.
#' @param courses_file Pfad zur CSV-Datei mit Kursdaten.
#' @return Eine Liste mit zwei Elementen: `students` und `courses` als data.frames.
#' @export
#' @importFrom utils read.csv head write.csv
#' @importFrom stats runif aggregate
import_data <- function(students_file, courses_file) {
  if (!file.exists(students_file)) stop(sprintf("Die Datei '%s' konnte nicht gefunden werden.", students_file))
  if (!file.exists(courses_file)) stop(sprintf("Die Datei '%s' konnte nicht gefunden werden.", courses_file))
  
  students <- read.csv(students_file, stringsAsFactors = FALSE)
  courses <- read.csv(courses_file, stringsAsFactors = FALSE)
  
  # 1. Kurse Validierung
  req_course_cols <- c("course_id", "min_capacity", "max_capacity")
  missing_c_cols <- setdiff(req_course_cols, names(courses))
  if (length(missing_c_cols) > 0) {
    stop(sprintf("FEHLER in Kurs-Daten: Es fehlen zwingend benoetigte Spalten: %s", paste(missing_c_cols, collapse = ", ")))
  }
  if (any(duplicated(courses$course_id))) {
    stop("FEHLER in Kurs-Daten: Die 'course_id' muss absolut eindeutig sein (keine Duplikate).")
  }
  if (!is.numeric(courses$min_capacity) || !is.numeric(courses$max_capacity)) {
    stop("FEHLER in Kurs-Daten: 'min_capacity' und 'max_capacity' muessen Zahlen sein.")
  }
  # Optionale aber empfohlene Spalte pruefen
  if (!"course_name" %in% names(courses)) {
    warning("HINWEIS: Die Spalte 'course_name' fehlt in den Kurs-Daten. Die Ausgabe im Dashboard wird nur IDs anzeigen.")
    courses$course_name <- courses$course_id
  }
  
  # 2. Schueler Validierung
  req_student_cols <- c("student_id", "first_choice", "second_choice", "third_choice")
  missing_s_cols <- setdiff(req_student_cols, names(students))
  if (length(missing_s_cols) > 0) {
    stop(sprintf("FEHLER in Schueler-Daten: Es fehlen zwingend benoetigte Spalten: %s", paste(missing_s_cols, collapse = ", ")))
  }
  if (any(duplicated(students$student_id))) {
    stop("FEHLER in Schueler-Daten: Die 'student_id' muss absolut eindeutig sein.")
  }
  # Optionaler Name
  if (!"student_name" %in% names(students)) {
    warning("HINWEIS: Die Spalte 'student_name' fehlt in den Schueler-Daten. Im Export wird nur die ID angezeigt.")
    students$student_name <- students$student_id
  }
  
  # Validierung, ob die gewaehlten Kurse ueberhaupt in der Kursliste existieren
  all_choices <- unique(c(students$first_choice, students$second_choice, students$third_choice))
  invalid_choices <- setdiff(all_choices, courses$course_id)
  if (length(invalid_choices) > 0 && !all(is.na(invalid_choices) | invalid_choices == "")) {
    invalid_sample <- head(invalid_choices, 5)
    stop(sprintf("FEHLER in Schueler-Daten: Schueler haben Kurse gewaehlt, die in der Kurs-Datei nicht existieren (z.B. %s)", paste(invalid_sample, collapse = ", ")))
  }
  
  # Tie-Breaker pruefen
  if (!"tie_breaker" %in% names(students)) {
    message("HINWEIS: Die Spalte 'tie_breaker' fehlt in den Schueler-Daten. Sie wird nun automatisch generiert, um faire Gleichstaende aufzuloesen.")
    students$tie_breaker <- runif(nrow(students), min = 0.001, max = 0.099)
  } else if (!is.numeric(students$tie_breaker)) {
    stop("FEHLER in Schueler-Daten: Die Spalte 'tie_breaker' muss Zahlen enthalten (z.B. Werte zwischen 0.001 und 0.099).")
  }
  
  return(list(students = students, courses = courses))
}

#' Ergebnisse exportieren
#'
#' Speichert die berechneten Zuweisungen und eine Kurs-Zusammenfassung als gut lesbare CSV-Dateien ab.
#'
#' @param result Das Ergebnis-Objekt von `optimize_courses`.
#' @param courses Der Kurs data.frame (fuer schoenere Namen im Export).
#' @param students Der Schueler data.frame (fuer die Ermittlung der Wahl-Nummer).
#' @param output_dir Der Ordner, in den die Dateien gespeichert werden sollen (Standard: aktuelles Verzeichnis).
#' @export
export_results <- function(result, courses, students, output_dir = ".") {
  if (result$status == "error" && nrow(result$assignments) == 0) {
    stop("Keine validen Ergebnisse zum Exportieren vorhanden (Solver-Fehler ohne Zwischenloesung).")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  assignments <- result$assignments
  
  # Wahl-Nummer und Schueler-Namen ermitteln
  # Da students jetzt optional student_name hat, nehmen wir das mit
  cols_students <- intersect(names(students), c("student_id", "student_name", "first_choice", "second_choice", "third_choice"))
  assignments <- merge(assignments, students[, cols_students], by="student_id", all.x=TRUE)
  
  assignments$choice_num <- "Zufall/Reste"
  assignments$choice_num[assignments$course_id == assignments$first_choice] <- "1. Wahl"
  assignments$choice_num[assignments$course_id == assignments$second_choice] <- "2. Wahl"
  assignments$choice_num[assignments$course_id == assignments$third_choice] <- "3. Wahl"
  
  # Fuer Lesbarkeit Kurs-Namen anfuegen, falls vorhanden
  if ("course_name" %in% names(courses)) {
    assignments <- merge(assignments, courses[, c("course_id", "course_name")], by = "course_id", all.x = TRUE)
    # Spalten sinnvoll sortieren
    if ("student_name" %in% names(assignments)) {
      assignments <- assignments[, c("student_id", "student_name", "course_id", "course_name", "choice_num")]
    } else {
      assignments <- assignments[, c("student_id", "course_id", "course_name", "choice_num")]
    }
  } else {
    if ("student_name" %in% names(assignments)) {
      assignments <- assignments[, c("student_id", "student_name", "course_id", "choice_num")]
    } else {
      assignments <- assignments[, c("student_id", "course_id", "choice_num")]
    }
  }
  
  # Sortieren nach Kurs und Schueler
  assignments <- assignments[order(assignments$course_id, assignments$student_id), ]
  
  # CSV 1: Detaillierte Zuweisungen
  file_assign <- file.path(output_dir, "zuweisungen_schueler.csv")
  write.csv(assignments, file_assign, row.names = FALSE, na = "")
  
  # CSV 2: Kurs-Statistik generieren
  course_counts <- aggregate(student_id ~ course_id, data = assignments, FUN = length)
  names(course_counts)[2] <- "assigned_students"
  
  course_summary <- merge(courses, course_counts, by = "course_id", all.x = TRUE)
  course_summary$assigned_students[is.na(course_summary$assigned_students)] <- 0
  
  course_summary$status <- "OK"
  course_summary$status[course_summary$assigned_students < course_summary$min_capacity] <- "Ausgefallen (< Min)"
  course_summary$status[course_summary$assigned_students == course_summary$max_capacity] <- "Ausgebucht (Max)"
  
  file_summary <- file.path(output_dir, "kurs_zusammenfassung.csv")
  write.csv(course_summary, file_summary, row.names = FALSE, na = "")
  
  message(sprintf("Erfolgreich exportiert nach '%s':\n 1) %s\n 2) %s", output_dir, basename(file_assign), basename(file_summary)))
}
