# End-to-End Simulation: Kurswahl-Optimierung
cat("Lade Paket und Abhängigkeiten...\n")
source("R/data_generator.R")
source("R/optimize.R")
source("R/dashboard.R")
suppressPackageStartupMessages({
  library(ompr)
  library(ompr.roi)
  library(ROI.plugin.glpk)
})

cat("Generiere Dummy-Daten...\n")
set.seed(42) # Für reproduzierbare Ergebnisse

# 50 reale Kurse generieren
kurse <- generate_courses(min_cap = 4, max_cap = 30)

# 300 Schüler generieren (inkl. Geschlecht und spez. Gewichtung)
schueler <- generate_students(n = 300, courses = kurse)

cat("Starte Optimierung OHNE Lebenserhaltungs-Flag (Limit: 45s)...\n")
start_time <- Sys.time()
ergebnis_normal <- optimize_courses(schueler, kurse, enforce_survival = FALSE, time_limit_sec = 45)
end_time <- Sys.time()
cat(sprintf("Dauer: %.2f Sekunden\n\n", as.numeric(difftime(end_time, start_time, units="secs"))))

evaluate_dashboard(ergebnis_normal, schueler, kurse)

cat("\n\n#####################################################\n")
cat("Starte Optimierung MIT Lebenserhaltungs-Flag (Limit: 45s)...\n")
start_time <- Sys.time()
ergebnis_flag <- optimize_courses(schueler, kurse, enforce_survival = TRUE, time_limit_sec = 45)
end_time <- Sys.time()
cat(sprintf("Dauer: %.2f Sekunden\n\n", as.numeric(difftime(end_time, start_time, units="secs"))))

evaluate_dashboard(ergebnis_flag, schueler, kurse)
