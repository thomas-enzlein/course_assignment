#' Daten importieren und validieren
#'
#' Liest Schueler- und Kursdaten aus einer Excel-Datei (.xlsx) ein.
#' Die Datei muss zwei Reiter haben: "Schüler" und "Kurse".
#' Falls kein `tie_breaker` vorhanden ist, wird dieser automatisch generiert.
#'
#' @param file_path Pfad zur Excel-Datei (.xlsx).
#' @return Eine Liste mit zwei Elementen: `students` und `courses` als data.frames.
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom utils head write.csv
#' @importFrom stats runif aggregate
import_data <- function(file_path) {
  if (!file.exists(file_path)) stop(sprintf("Die Datei '%s' konnte nicht gefunden werden.", file_path))
  
  sheets <- readxl::excel_sheets(file_path)
  
  # Suche nach passenden Reitern (Case-insensitive)
  s_sheet <- sheets[tolower(sheets) %in% c("schüler", "schueler", "students")]
  c_sheet <- sheets[tolower(sheets) %in% c("kurse", "courses")]
  
  if (length(s_sheet) == 0) stop("Die Excel-Datei muss einen Reiter 'Schüler' enthalten.")
  if (length(c_sheet) == 0) stop("Die Excel-Datei muss einen Reiter 'Kurse' enthalten.")
  
  students <- as.data.frame(readxl::read_excel(file_path, sheet = s_sheet[1]))
  courses <- as.data.frame(readxl::read_excel(file_path, sheet = c_sheet[1]))

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

  # 3. Spalten standardisieren (Gender & Klasse)
  # Gender Aliases
  gender_alias <- c("gender", "geschlecht", "m/w", "sex")
  found_gender <- names(students)[tolower(names(students)) %in% gender_alias]
  if (length(found_gender) > 0) {
    names(students)[names(students) == found_gender[1]] <- "gender"
    # Werte vereinheitlichen (m/w)
    students$gender <- tolower(as.character(students$gender))
    students$gender[students$gender %in% c("m\u00e4nnlich", "male", "m", "1")] <- "m"
    students$gender[students$gender %in% c("weiblich", "female", "w", "2", "f")] <- "w"
  } else {
    students$gender <- "k.A." # keine Angabe
  }

  # Class Aliases
  class_alias <- c("class", "klasse", "stufe", "jahrgang")
  found_class <- names(students)[tolower(names(students)) %in% class_alias]
  if (length(found_class) > 0) {
    names(students)[names(students) == found_class[1]] <- "class"
  } else {
    students$class <- "k.A."
  }

  # Optionaler Klassen-Name (Klasse oder class)
  if ("Klasse" %in% names(students) && !"class" %in% names(students)) {
    students$class <- students$Klasse
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
#' Speichert die berechneten Zuweisungen und eine Kurs-Zusammenfassung als CSV-Dateien.
#'
#' @param result Das Ergebnis-Objekt von `optimize_courses`.
#' @param courses Der Kurs data.frame (fuer Namen im Export).
#' @param students Der Schueler data.frame (fuer die Ermittlung der Wahl-Prioritaet).
#' @param output_dir Das Zielverzeichnis (Standard: ".").
#' @export
export_results <- function(result, courses, students, output_dir = ".") {
  if (nrow(result$assignments) == 0) {
    stop("Keine validen Ergebnisse zum Exportieren vorhanden (Solver-Fehler ohne Zwischenloesung).")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  assignments <- result$assignments

  # Wahl-Nummer und Schueler-Namen ermitteln
  # Da students jetzt optional student_name und class/Klasse hat, nehmen wir das mit
  cols_students <- intersect(names(students), c("student_id", "student_name", "class", "Klasse", "first_choice", "second_choice", "third_choice"))
  assignments <- merge(assignments, students[, cols_students], by = "student_id", all.x = TRUE)

  assignments$choice_num <- "Zufall/Reste"
  assignments$choice_num[assignments$course_id == assignments$first_choice] <- "1. Wahl"
  assignments$choice_num[assignments$course_id == assignments$second_choice] <- "2. Wahl"
  assignments$choice_num[assignments$course_id == assignments$third_choice] <- "3. Wahl"

  # Fuer Lesbarkeit Kurs-Namen anfuegen, falls vorhanden
  if ("course_name" %in% names(courses)) {
    assignments <- merge(assignments, courses[, c("course_id", "course_name")], by = "course_id", all.x = TRUE)
  }
  
  # Spalten sinnvoll sortieren
  final_assign_cols <- c("student_id")
  if ("student_name" %in% names(assignments)) final_assign_cols <- c(final_assign_cols, "student_name")
  
  # Behandle class oder Klasse
  if ("class" %in% names(assignments)) {
    final_assign_cols <- c(final_assign_cols, "class")
  } else if ("Klasse" %in% names(assignments)) {
    final_assign_cols <- c(final_assign_cols, "Klasse")
  }

  final_assign_cols <- c(final_assign_cols, "course_id")
  if ("course_name" %in% names(assignments)) final_assign_cols <- c(final_assign_cols, "course_name")
  final_assign_cols <- c(final_assign_cols, "choice_num")
  
  assignments <- assignments[, final_assign_cols]

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

#' Kurs-Kapazitaeten aktualisieren
#'
#' Hilfsfunktion zur Validierung und Aktualisierung von Kurs-Kapazitaeten.
#' Wird primaer im Shiny-Kontext fuer editierbare Tabellen genutzt.
#'
#' @param courses data.frame mit Kursdaten.
#' @param row_idx Zeilenindex.
#' @param col_name Spaltenname ("min_capacity" oder "max_capacity").
#' @param value Neuer Wert (muss positiver Integer sein).
#' @return Aktualisierter data.frame.
#' @export
update_course_capacity <- function(courses, row_idx, col_name, value) {
  if (is.null(courses)) stop("Kurs-Daten fehlen.")
  if (row_idx < 1 || row_idx > nrow(courses)) stop("Ungueltiger Zeilenindex.")
  if (!col_name %in% c("min_capacity", "max_capacity")) stop("Ungueltiger Spaltenname.")

  val <- as.integer(value)
  if (is.na(val) || val < 0) stop("Wert muss eine positive Ganzzahl sein.")

  courses[row_idx, col_name] <- val
  return(courses)
}
