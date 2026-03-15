#' Kurse generieren
#'
#' Erstellt eine Dummy-Tabelle fuer Kurse mit Profil-Gewichten.
#' Stress-Test-Version: Weniger Kurse (40) und knappere Kapazitaeten.
#'
#' @param min_cap_range Vektor mit min/max fuer die Mindestteilnehmerzahl (Standard: 4-8)
#' @return Ein data.frame mit Kursdaten
#' @export
generate_courses <- function(min_cap_range = c(4, 8)) {
  # Reduziert auf 40 Kurse für mehr Verdichtung
  courses_def <- data.frame(
    course_id = sprintf("C%02d", 1:40),
    course_name = c(
      "Mathematik-Olympiade", "Programmieren in Python", "Web-Design & UI", 
      "Robotik-Team", "Quantenphysik-Experimente", "Chemie-Labor", 
      "Molekularbiologie", "Astronomie & Teleskopie", "Spanisch Konversation", 
      "Business-Französisch", "Latein-Exkursionen", "Altgriechisch", 
      "English Debate Club", "Creative Writing", "Investigativer Journalismus",
      "Musical & Schauspiel", "Bühnenbild & Lichttechnik", "Großer Schulchor", 
      "Jazz-Band", "Acrylmalerei & Öltechnik", "Digitalfotografie", 
      "Moderne Töpferei", "Zeitgeschichte: 20. Jhd", "Politik & Debatten", 
      "Angewandte Philosophie", "Praktische Psychologie", "Soziologie & Gesellschaft", 
      "Geographie & Klimawandel", "Fine Dining & Kochen", "Fashion Design & Nähen",
      "Fußball-Akademie", "Basketball Masters", "Beachvolleyball", 
      "Leichtathletik", "Schwimmwettbewerb", "Contemporary Dance", 
      "Yoga & Achtsamkeit", "Indoor-Klettern", "Sanitäter & Erste Hilfe", 
      "Schach-Großmeister"
    ),
    category = c(
      "MINT", "MINT", "MINT", "MINT", "MINT", "MINT", "MINT", "MINT",
      "SPRACHEN", "SPRACHEN", "SPRACHEN", "SPRACHEN", "SPRACHEN", "SPRACHEN", "SPRACHEN",
      "KREATIV", "KREATIV", "KREATIV", "KREATIV", "KREATIV", "KREATIV", "KREATIV",
      "SOZIAL", "SOZIAL", "SOZIAL", "SOZIAL", "SOZIAL", "SOZIAL",
      "MIX", "MIX",
      "SPORT", "SPORT", "SPORT", "SPORT", "SPORT", "SPORT", "SPORT", "SPORT",
      "SOZIAL", "MIX"
    ),
    stringsAsFactors = FALSE
  )

  # Zufällige Mindestkapazität zwischen 4 und 8
  set.seed(123) # Reproduzierbare Simulation
  courses_def$min_capacity <- sample(min_cap_range[1]:min_cap_range[2], nrow(courses_def), replace = TRUE)
  
  # STRESS-MODUS 3.1: 
  # 300 Schüler / 40 Kurse. 
  # Wir setzen die Kapazitäten auf 8 bis 15 Plätze. 
  # Erwartete Gesamtkapazität: ca. 450 Plätze für 300 Schüler.
  # Damit gibt es genug Plätze, aber durch Hype-Kurse und Mindestgrößen 
  # entstehen trotzdem spannende Konflikte.
  courses_def$max_capacity <- sample(8:15, nrow(courses_def), replace = TRUE)

  # Härtefälle: Die "Hype-Kurse" bleiben knapp
  bottleneck_courses <- c(
    "Programmieren in Python", "Robotik-Team", "Fine Dining & Kochen", 
    "Fußball-Akademie", "Musical & Schauspiel", "Indoor-Klettern"
  )
  courses_def$max_capacity[courses_def$course_name %in% bottleneck_courses] <- 7
  courses_def$min_capacity[courses_def$course_name %in% bottleneck_courses] <- 5

  return(courses_def)
}

#' Schueler mit realistischen Profilen generieren
#'
#' @param n Anzahl der Schueler (Standard: 300)
#' @param courses data.frame mit Kursdaten
#' @return Ein data.frame mit Schuelerpraeferenzen
#' @export
generate_students <- function(n = 300, courses) {
  vornamen_m <- c(
    "Leon", "Noah", "Finn", "Elias", "Emil", "Luca", "Ben", "Luis", "Jonas", "Lukas",
    "Felix", "Theo", "Matteo", "Liam", "Anton", "Jakob", "Paul", "Maximilian", "Leo", "Moritz",
    "Felix", "Henry", "Julian", "Jan", "Levin", "Samuel", "David", "Linus", "Konstantin", "Oscar"
  )
  vornamen_w <- c(
    "Mia", "Emma", "Sofia", "Hannah", "Emilia", "Anna", "Mila", "Lina", "Ella", "Leni",
    "Clara", "Marie", "Lea", "Lara", "Maja", "Sophie", "Frieda", "Ida", "Johanna", "Mathilda",
    "Greta", "Nora", "Lotta", "Paula", "Romy", "Marlene", "Lia", "Elena", "Isabella", "Sarah"
  )
  nachnamen <- c(
    "Müller", "Schmidt", "Schneider", "Fischer", "Meyer", "Weber", "Schulz", "Wagner", "Becker", "Hoffmann",
    "Bauer", "Richter", "Klein", "Wolf", "Schröder", "Neumann", "Schwarz", "Zimmermann", "Braun", "Krüger"
  )

  genders <- sample(c("m", "w"), size = n, replace = TRUE)
  archetypes <- sample(
    c("Allrounder", "Sportler", "MINT-Techie", "Kreativer", "Sprach-Sozial"),
    size = n, replace = TRUE, 
    prob = c(0.25, 0.25, 0.2, 0.15, 0.15)
  )

  results <- vector("list", n)
  
  for (i in 1:n) {
    vor <- if (genders[i] == "m") sample(vornamen_m, 1) else sample(vornamen_w, 1)
    nach <- sample(nachnamen, 1)
    
    weights <- rep(1, nrow(courses))
    
    # EXTREMER Hype-Faktor (Kochen, E-Sports, Fußball sind immer massiv überbucht)
    hype_courses <- c("Fine Dining & Kochen", "E-Sports League", "Fußball-Akademie", "Musical & Schauspiel")
    weights[courses$course_name %in% hype_courses] <- weights[courses$course_name %in% hype_courses] * 15
    
    profile <- archetypes[i]
    if (profile == "Sportler") {
      weights[courses$category == "SPORT"] <- weights[courses$category == "SPORT"] * 18
    } else if (profile == "MINT-Techie") {
      weights[courses$category == "MINT"] <- weights[courses$category == "MINT"] * 18
    } else if (profile == "Kreativer") {
      weights[courses$category == "KREATIV"] <- weights[courses$category == "KREATIV"] * 18
    } else if (profile == "Sprach-Sozial") {
      weights[courses$category == "SPRACHEN" | courses$category == "SOZIAL"] <- 
        weights[courses$category == "SPRACHEN" | courses$category == "SOZIAL"] * 15
    }
    
    # Sanfte Geschlechter-Tendenz
    if (genders[i] == "w") {
      w_fav <- c("Contemporary Dance", "Fashion Design & Nähen", "Sanitäter & Erste Hilfe")
      weights[courses$course_name %in% w_fav] <- weights[courses$course_name %in% w_fav] * 2.5
    } else {
      m_fav <- c("Basketball Masters", "Fußball-Akademie")
      weights[courses$course_name %in% m_fav] <- weights[courses$course_name %in% m_fav] * 2.5
    }

    weights <- weights * runif(length(weights), 0.4, 2.5)
    s_choices <- sample(courses$course_id, size = 3, replace = FALSE, prob = weights)
    
    results[[i]] <- data.frame(
      student_id = sprintf("S%03d", i),
      student_name = paste(vor, nach),
      gender = genders[i],
      first_choice = s_choices[1],
      second_choice = s_choices[2],
      third_choice = s_choices[3],
      tie_breaker = runif(1, 0.001, 0.099),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}
