#' Optimierungs-Ergebnisse auswerten
#'
#' Zeigt eine Zusammenfassung der Schuelerzufriedenheit und des Kursstatus im Terminal an.
#' Berechnet zudem detaillierte Statistiken fuer die Darstellung im Dashboard.
#'
#' @param res Das Ergebnis-Objekt der Funktion optimize_courses()
#' @param students Der data.frame der Schueler
#' @param courses Der data.frame der Kurse
#' @return Eine unsichtbare Liste mit:
#'   \item{choice_1, choice_2, choice_3}{Anzahl der Schueler mit jeweiliger Wahl}
#'   \item{unassigned}{Anzahl der nicht zugewiesenen Schueler}
#'   \item{course_stats}{data.frame mit Teilnehmerzahlen und Gesamtnachfrage pro Kurs}
#'   \item{rest_students}{data.frame der nicht zugewiesenen Schueler}
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
    
    # NEU: Gender-Split pro Kurs
    if (!"gender" %in% names(students)) {
      students$gender <- "k.A."
    }
    
    ass_with_gender <- merge(assignments, students[, c("student_id", "gender")], by = "student_id")
    gender_counts <- as.data.frame(table(ass_with_gender$course_id, ass_with_gender$gender))
    if (nrow(gender_counts) > 0) {
      names(gender_counts) <- c("course_id", "gender", "count")
      gender_counts$course_id <- as.character(gender_counts$course_id)
      
      counts_m <- gender_counts[gender_counts$gender == "m", c("course_id", "count")]
      names(counts_m)[2] <- "participants_m"
      
      counts_w <- gender_counts[gender_counts$gender == "w", c("course_id", "count")]
      names(counts_w)[2] <- "participants_w"
      
      counts <- merge(counts, counts_m, by = "course_id", all.x = TRUE)
      counts <- merge(counts, counts_w, by = "course_id", all.x = TRUE)
      
      # NEU: Alles andere (k.A. oder sonstige)
      counts$participants_m[is.na(counts$participants_m)] <- 0
      counts$participants_w[is.na(counts$participants_w)] <- 0
      counts$participants_other <- counts$participants - counts$participants_m - counts$participants_w
    } else {
      counts$participants_m <- 0
      counts$participants_w <- 0
      counts$participants_other <- counts$participants
    }
  } else {
    counts <- data.frame(
      course_id = character(), 
      participants = integer(), 
      participants_m = integer(), 
      participants_w = integer(),
      participants_other = integer(),
      stringsAsFactors = FALSE
    )
  }

  # Merge mit allen Kursen
  course_stats <- merge(courses, counts, by = "course_id", all.x = TRUE)
  course_stats <- merge(course_stats, interest_counts, by = "course_id", all.x = TRUE)
  
  course_stats$participants[is.na(course_stats$participants)] <- 0
  course_stats$participants_m[is.na(course_stats$participants_m)] <- 0
  course_stats$participants_w[is.na(course_stats$participants_w)] <- 0
  course_stats$participants_other[is.na(course_stats$participants_other)] <- 0
  course_stats$total_interest[is.na(course_stats$total_interest)] <- 0

  # Detaillierte Spalten fuer die UI (Immer mit korrekter Laenge initialisieren)
  n_rest <- nrow(rest_students)
  rest_students$reason_1 <- rep("-", n_rest)
  rest_students$reason_2 <- rep("-", n_rest)
  rest_students$reason_3 <- rep("-", n_rest)

  # --- ENHANCEMENT: WHY WERE THEY NOT ASSIGNED? ---
  if (nrow(rest_students) > 0) {
    rest_students$reasons <- vapply(seq_len(nrow(rest_students)), function(i) {
      wishes <- c(rest_students$first_choice[i], rest_students$second_choice[i], rest_students$third_choice[i])
      st_reasons <- vapply(wishes, function(cid) {
        if (is.na(cid) || cid == "") return("-")
        st <- course_stats[course_stats$course_id == cid, ]
        if (nrow(st) == 0) return("ID nicht gefunden")
        if (st$participants[1] == 0) return("Entf\u00e4llt (< Min)")
        if (st$participants[1] >= st$max_capacity[1]) return("Voll (Max)")
        return("Planungskonflikt")
      }, character(1))
      paste(st_reasons, collapse = " | ")
    }, character(1))
    
    rest_students$reason_1 <- vapply(rest_students$first_choice, function(cid) {
       if (is.na(cid) || cid == "") return("-")
       st <- course_stats[course_stats$course_id == cid, ]
       if (nrow(st) == 0) return("ID nicht gefunden")
       if (st$participants[1] == 0) return("Entf\u00e4llt")
       if (st$participants[1] >= st$max_capacity[1]) return("Voll")
       "Konflikt"
    }, character(1))
    rest_students$reason_2 <- vapply(rest_students$second_choice, function(cid) {
       if (is.na(cid) || cid == "") return("-")
       st <- course_stats[course_stats$course_id == cid, ]
       if (nrow(st) == 0) return("ID nicht gefunden")
       if (st$participants[1] == 0) return("Entf\u00e4llt")
       if (st$participants[1] >= st$max_capacity[1]) return("Voll")
       "Konflikt"
    }, character(1))
    rest_students$reason_3 <- vapply(rest_students$third_choice, function(cid) {
       if (is.na(cid) || cid == "") return("-")
       st <- course_stats[course_stats$course_id == cid, ]
       if (nrow(st) == 0) return("ID nicht gefunden")
       if (st$participants[1] == 0) return("Entf\u00e4llt")
       if (st$participants[1] >= st$max_capacity[1]) return("Voll")
       "Konflikt"
    }, character(1))
  }

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

  # Gender-split satisfaction
  cols_needed <- c("student_id", "gender", "first_choice", "second_choice", "third_choice")
  cols_to_merge <- intersect(names(students), cols_needed)
  ass_with_gender <- merge(assignments, students[, cols_to_merge, drop = FALSE], by = "student_id", all.x = TRUE)
  rest_with_gender <- rest_students # rest_students already comes from students and has all columns
  
  choice_1_m <- sum(ass_with_gender$gender == "m" & ass_with_gender$course_id == ass_with_gender$first_choice, na.rm = TRUE)
  choice_1_w <- sum(ass_with_gender$gender == "w" & ass_with_gender$course_id == ass_with_gender$first_choice, na.rm = TRUE)
  choice_1_other <- choice_1 - choice_1_m - choice_1_w
  
  choice_2_m <- sum(ass_with_gender$gender == "m" & ass_with_gender$course_id == ass_with_gender$second_choice, na.rm = TRUE)
  choice_2_w <- sum(ass_with_gender$gender == "w" & ass_with_gender$course_id == ass_with_gender$second_choice, na.rm = TRUE)
  choice_2_other <- choice_2 - choice_2_m - choice_2_w
  
  choice_3_m <- sum(ass_with_gender$gender == "m" & ass_with_gender$course_id == ass_with_gender$third_choice, na.rm = TRUE)
  choice_3_w <- sum(ass_with_gender$gender == "w" & ass_with_gender$course_id == ass_with_gender$third_choice, na.rm = TRUE)
  choice_3_other <- choice_3 - choice_3_m - choice_3_w
  
  unassigned_m <- sum(rest_with_gender$gender == "m", na.rm = TRUE)
  unassigned_w <- sum(rest_with_gender$gender == "w", na.rm = TRUE)
  unassigned_other <- unassigned - unassigned_m - unassigned_w

  invisible(list(
    choice_1 = choice_1, choice_2 = choice_2, choice_3 = choice_3, unassigned = unassigned,
    choice_1_m = choice_1_m, choice_1_w = choice_1_w, choice_1_other = choice_1_other,
    choice_2_m = choice_2_m, choice_2_w = choice_2_w, choice_2_other = choice_2_other,
    choice_3_m = choice_3_m, choice_3_w = choice_3_w, choice_3_other = choice_3_other,
    unassigned_m = unassigned_m, unassigned_w = unassigned_w, unassigned_other = unassigned_other,
    course_stats = course_stats, rest_students = rest_students
  ))
}
