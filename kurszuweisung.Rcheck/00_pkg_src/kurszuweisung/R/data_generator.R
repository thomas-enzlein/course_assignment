#' Kurse generieren
#'
#' Erstellt eine Dummy-Tabelle fuer Kurse mit festen Namen und geschlechtsspezifischen
#' Beliebtheits-Gewichten basierend auf einer Normalverteilung.
#' 
#' @param min_cap Mindestteilnehmerzahl (Standard: 4)
#' @param max_cap Maximalteilnehmerzahl (Standard: 30)
#' @return Ein data.frame mit Kursdaten und Gewichten
#' @export
#' @importFrom stats rnorm
generate_courses <- function(min_cap = 4, max_cap = 30) {
  # Definierte 50 Kurse mit Basis-Popularitaet (m = Maennlich, w = Weiblich)
  # Kategorien: "sehr beliebt", "beliebt", "speziell"
  courses_def <- data.frame(
    course_id = sprintf("C%03d", 1:50),
    course_name = c(
      "Mathematik-Olympiade", "Informatik (Programmieren)", "Web-Design", "Robotik", "Physik-Experimente",
      "Chemie Labor", "Biologie - Oekosystem Wald", "Astronomie", "Spanisch fuer Anfaenger", "Franzoesisch Konversation",
      "Latein Lektuere", "Altgriechisch", "English Debate Club", "Kreatives Schreiben", "Schuelerzeitung",
      "Theatergruppe (Schauspiel)", "Buehnenbild & Technik", "Chor", "Schulband", "Kunst: Acrylmalerei",
      "Kunst: Fotografie", "Toepferei", "Geschichte: Der Zweite Weltkrieg", "Politik und Wirtschaft", "Philosophie (Ethik)",
      "Psychologie", "Soziologie", "Geographie: Klimawandel", "Kochen & Backen", "Naehen & Textildesign",
      "Fussball", "Basketball", "Volleyball", "Leichtathletik", "Schwimmen",
      "Tanz (Hip-Hop)", "Yoga & Entspannung", "Klettern", "Erste Hilfe (Sanitaeter)", "Schachclub",
      "Brettspiele & Strategie", "E-Sports AG", "Gartenarbeit (Schulgarten)", "Tierschutz AG", "Mediation & Streitschlichtung",
      "Jugend forscht", "Wirtschaft - StartUp-Gruendung", "Eventmanagement (Schulfeste)", "Jahrbuch-Redaktion", "Gitarre fuer Anfaenger"
    ),
    pop_m = c(
      "speziell", "sehr beliebt", "beliebt", "sehr beliebt", "beliebt",
      "beliebt", "speziell", "speziell", "speziell", "speziell",
      "speziell", "speziell", "speziell", "speziell", "speziell",
      "speziell", "beliebt", "speziell", "beliebt", "speziell",
      "beliebt", "speziell", "beliebt", "beliebt", "speziell",
      "speziell", "speziell", "beliebt", "beliebt", "speziell",
      "sehr beliebt", "sehr beliebt", "beliebt", "beliebt", "beliebt",
      "speziell", "speziell", "beliebt", "beliebt", "beliebt",
      "beliebt", "sehr beliebt", "speziell", "speziell", "speziell",
      "beliebt", "beliebt", "speziell", "speziell", "beliebt"
    ),
    pop_w = c(
      "speziell", "speziell", "beliebt", "speziell", "speziell",
      "beliebt", "beliebt", "speziell", "beliebt", "beliebt",
      "speziell", "speziell", "beliebt", "sehr beliebt", "beliebt",
      "sehr beliebt", "speziell", "sehr beliebt", "beliebt", "sehr beliebt",
      "beliebt", "beliebt", "speziell", "speziell", "beliebt",
      "sehr beliebt", "beliebt", "beliebt", "sehr beliebt", "sehr beliebt",
      "beliebt", "beliebt", "sehr beliebt", "beliebt", "beliebt",
      "sehr beliebt", "sehr beliebt", "beliebt", "beliebt", "speziell",
      "speziell", "speziell", "beliebt", "sehr beliebt", "beliebt",
      "speziell", "speziell", "beliebt", "beliebt", "beliebt"
    ),
    stringsAsFactors = FALSE
  )
  
  # Hilfsfunktion zur Generierung von Normalverteilten Gewichten
  get_weight <- function(pop_class) {
    w <- numeric(length(pop_class))
    
    idx_sehr <- pop_class == "sehr beliebt"
    w[idx_sehr] <- rnorm(sum(idx_sehr), mean = 50, sd = 10)
    
    idx_bel <- pop_class == "beliebt"
    w[idx_bel] <- rnorm(sum(idx_bel), mean = 10, sd = 2)
    
    idx_spez <- pop_class == "speziell"
    w[idx_spez] <- rnorm(sum(idx_spez), mean = 1.5, sd = 0.5)
    
    # Absicherung, dass keine negativen Gewichte entstehen
    pmax(0.1, w)
  }
  
  courses_def$weight_m <- get_weight(courses_def$pop_m)
  courses_def$weight_w <- get_weight(courses_def$pop_w)
  
  courses_def$min_capacity <- min_cap
  courses_def$max_capacity <- max_cap
  
  # Bestimmte Raum-Limits festlegen (z.B. Labore, EDV-Raeume) auf 15 Plaetze reduzieren
  small_rooms <- c(
    "Informatik (Programmieren)", "Robotik", "Chemie Labor", "Toepferei", 
    "Kunst: Acrylmalerei", "Kochen \u0026 Backen", "Naehen \u0026 Textildesign",
    "Web-Design", "Physik-Experimente", "Kunst: Fotografie"
  )
  courses_def$max_capacity[courses_def$course_name %in% small_rooms] <- 15
  
  return(courses_def)
}

#' Schueler mit Praeferenzen generieren
#'
#' Generiert Schuelerdaten inkl. Geschlecht (m/w), 1., 2. und 3. Wahl 
#' sowie einen Tie-Breaker.
#'
#' @param n Anzahl der Schueler (Standard: 300)
#' @param courses Ein data.frame mit Kursen und Gewichten (weight_m, weight_w)
#' @return Ein data.frame mit Schuelerpraeferenzen und Tie-Breaker
#' @importFrom stats runif
generate_students <- function(n = 300, courses) {
  if (nrow(courses) < 3) stop("Es muessen mindestens 3 Kurse vorhanden sein.")
  if (!all(c("course_id", "weight_m", "weight_w") %in% names(courses))) {
    stop("courses muss course_id, weight_m und weight_w enthalten.")
  }
  
  genders <- sample(c("m", "w"), size = n, replace = TRUE)
  
  # Vorlagen fuer Namen
  names_m <- c("Leon", "Finn", "Elias", "Jonas", "Luis", "Noah", "Paul", "Julian", "Max", "Lukas", "Felix", "Tim", "Moritz", "Linus", "Ben")
  names_w <- c("Mia", "Emma", "Hannah", "Sofia", "Anna", "Lea", "Emilia", "Marie", "Lina", "Mila", "Lara", "Nele", "Johanna", "Clara", "Laura")
  
  student_name <- character(n)
  
  first_choice <- character(n)
  second_choice <- character(n)
  third_choice <- character(n)
  
  for(i in 1:n) {
    # Geschlechtsspezifische Namen und Gewichte waehlen
    if (genders[i] == "m") {
      student_name[i] <- sample(names_m, 1)
      prob_weights <- courses$weight_m
    } else {
      student_name[i] <- sample(names_w, 1)
      prob_weights <- courses$weight_w
    }
    
    choices <- sample(courses$course_id, size = 3, replace = FALSE, prob = prob_weights)
    first_choice[i] <- choices[1]
    second_choice[i] <- choices[2]
    third_choice[i] <- choices[3]
  }
  
  # Tie-Breaker (0.001 bis 0.099)
  tie_breaker <- runif(n, min = 0.001, max = 0.099)
  
  data.frame(
    student_id = sprintf("S%03d", 1:n),
    student_name = student_name,
    gender = genders,
    first_choice = first_choice,
    second_choice = second_choice,
    third_choice = third_choice,
    tie_breaker = tie_breaker,
    stringsAsFactors = FALSE
  )
}
