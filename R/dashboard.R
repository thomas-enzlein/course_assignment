#' Optimierungs-Ergebnisse auswerten
#'
#' Zeigt eine Zusammenfassung der Schuelerzufriedenheit und des Kursstatus im Terminal an.
#'
#' @param res Das Ergebnis-Objekt der Funktion optimize_courses()
#' @param students Der data.frame der Schueler
#' @param courses Der data.frame der Kurse
#' @export
#' @importFrom utils head
evaluate_dashboard <- function(res, students, courses) {
  assignments <- res$assignments

  cat("====================================================\n")
  cat("              KURSWAHL AUSWERTUNG                   \n")
  cat("====================================================\n")

  display_status <- res$status
  if (display_status == "error" && nrow(assignments) > 0) {
    display_status <- "Abbruch durch Zeitlimit (Valide Zwischenloesung gefunden)"
  }
  cat(sprintf("Solver Status: %s\n", display_status))
  cat(sprintf("Gesamt-Score: %.2f\n\n", res$objective_value))

  # 1. Zufriedenheits-Metrik
  total_students <- nrow(students)
  assigned_students <- nrow(assignments)

  choice_1 <- 0
  choice_2 <- 0
  choice_3 <- 0

  if (assigned_students > 0) {
    for (i in seq_len(nrow(assignments))) {
      sid <- assignments$student_id[i]
      cid <- assignments$course_id[i]

      stud <- students[students$student_id == sid, ]
      if (stud$first_choice[1] == cid) choice_1 <- choice_1 + 1
      else if (stud$second_choice[1] == cid) choice_2 <- choice_2 + 1
      else if (stud$third_choice[1] == cid) choice_3 <- choice_3 + 1
    }
  }

  cat("--- ZUFRIEDENHEIT ---\n")
  cat(sprintf("1. Wahl erhalten: %4d (%5.1f %%)\n", choice_1, (choice_1 / total_students) * 100))
  cat(sprintf("2. Wahl erhalten: %4d (%5.1f %%)\n", choice_2, (choice_2 / total_students) * 100))
  cat(sprintf("3. Wahl erhalten: %4d (%5.1f %%)\n", choice_3, (choice_3 / total_students) * 100))
  unassigned <- total_students - assigned_students
  cat(sprintf("Nicht zugewiesen: %4d (%5.1f %%)\n\n", unassigned, (unassigned / total_students) * 100))

  # 2. Die Reste-Rampe
  cat("--- RESTE-RAMPE (Manuelle Nachbearbeitung) ---\n")
  assigned_ids <- assignments$student_id
  rest_students <- students[!students$student_id %in% assigned_ids, ]

  if (nrow(rest_students) == 0) {
    cat("Perfekt! Alle Schueler wurden erfolgreich zugewiesen.\n\n")
  } else {
    cat(sprintf("%d Schueler ohne Kurs-Zuweisung:\n", nrow(rest_students)))
    # Zeige nur die ersten 10
    head_rest <- head(rest_students, 10)
    for (i in seq_len(nrow(head_rest))) {
      cat(sprintf(" - %s (gewollt: %s, %s, %s)\n",
        head_rest$student_id[i], head_rest$first_choice[i],
        head_rest$second_choice[i], head_rest$third_choice[i]))
    }
    if (nrow(rest_students) > 10) cat("   ... und weitere.\n")
    cat("\n")
  }

  # 3. Kurs-Statistiken
  cat("--- KURS-STATUS ---\n")
  
  # Interesse berechnen (Wie viele Schueler hatten den Kurs als Wunsch?)
  all_wishes <- c(students$first_choice, students$second_choice, students$third_choice)
  interest_counts <- as.data.frame(table(all_wishes))
  names(interest_counts) <- c("course_id", "total_interest")
  interest_counts$course_id <- as.character(interest_counts$course_id)

  # Teilnehmende an jedem Kurs zaehlen
  if (assigned_students > 0) {
    counts <- as.data.frame(table(assignments$course_id))
    names(counts) <- c("course_id", "participants")
    counts$course_id <- as.character(counts$course_id)
  } else {
    counts <- data.frame(course_id = character(), participants = integer(), stringsAsFactors = FALSE)
  }

  # Merge mit allen Kursen
  course_stats <- merge(courses, counts, by = "course_id", all.x = TRUE)
  course_stats <- merge(course_stats, interest_counts, by = "course_id", all.x = TRUE)
  
  course_stats$participants[is.na(course_stats$participants)] <- 0
  course_stats$total_interest[is.na(course_stats$total_interest)] <- 0

  # Volle Kurse
  full_courses <- course_stats[course_stats$participants >= course_stats$max_capacity, ]
  cat(sprintf("Ausgebuchte Kurse: %d\n", nrow(full_courses)))

  # Ausgefallene Kurse (participants == 0)
  dead_courses <- course_stats[course_stats$participants == 0, ]
  cat(sprintf("Ausgefallene Kurse (< min): %d\n", nrow(dead_courses)))

  # Wackelkandidaten (exakt min_capacity)
  wackler <- course_stats[course_stats$participants > 0 & course_stats$participants == course_stats$min_capacity, ]
  cat(sprintf("Wackelkandidaten (exakt Min-Cap): %d\n", nrow(wackler)))
  if (nrow(wackler) > 0) {
    for (i in seq_len(nrow(wackler))) {
      cat(sprintf(" - %s (%s) hat exakt %d Teilnehmer.\n",
        wackler$course_id[i], wackler$course_name[i], wackler$participants[i]))
    }
  }
  cat("====================================================\n")

  invisible(list(
    choice_1 = choice_1, choice_2 = choice_2, choice_3 = choice_3, unassigned = unassigned,
    course_stats = course_stats, rest_students = rest_students
  ))
}
