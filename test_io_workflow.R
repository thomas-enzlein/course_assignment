# End-to-End Workflow-Test: IO Pipeline
cat("Lade Paket und Abhängigkeiten...\n")
source("R/data_generator.R")
source("R/optimize.R")
source("R/dashboard.R")
source("R/io.R")
suppressPackageStartupMessages({
  library(ompr)
  library(ompr.roi)
  library(ROI.plugin.glpk)
})

# 1. Daten generieren
cat("\n--- 1. Daten generieren ---\n")
set.seed(42)
kurse_gen <- generate_courses(min_cap = 4, max_cap = 30)
schueler_gen <- generate_students(n = 300, courses = kurse_gen)

# 2. Daten als CSV speichern (Simuliert den Excel-Export der Schule)
cat("\n--- 2. Daten speichern (Simulation Schul-Export) ---\n")
dir.create("Beispieldaten", showWarnings = FALSE)
kurse_file <- "Beispieldaten/kurse.csv"
schueler_file <- "Beispieldaten/schueler.csv"

# Wir speichern bewusst nur die relevanten Spalten ab
write.csv(kurse_gen[, c("course_id", "course_name", "min_capacity", "max_capacity")], kurse_file, row.names = FALSE)
write.csv(schueler_gen[, c("student_id", "student_name", "first_choice", "second_choice", "third_choice", "tie_breaker")], schueler_file, row.names = FALSE)
cat("Beispieldaten gespeichert in 'Beispieldaten/'.\n")

# 3. Daten via import_data() importieren und validieren
cat("\n--- 3. Daten importieren und validieren ---\n")
import_res <- import_data(schueler_file, kurse_file)
schueler_import <- import_res$students
kurse_import <- import_res$courses
cat("Daten erfolgreich und fehlerfrei importiert!\n")

# 4. Optimierung mit den importierten Daten durchführen
cat("\n--- 4. Optimierung durchführen ---\n")
cat("Starte Solver (Limit: 15s)...\n")
start_time <- Sys.time()
ergebnis <- optimize_courses(schueler_import, kurse_import, enforce_survival = TRUE, time_limit_sec = 15)
end_time <- Sys.time()
cat(sprintf("Dauer: %.2f Sekunden\n", as.numeric(difftime(end_time, start_time, units="secs"))))

# Optional: kurzes Dashboard
evaluate_dashboard(ergebnis, schueler_import, kurse_import)

# 5. Ergebnisse exportieren
cat("\n--- 5. Ergebnisse exportieren ---\n")
export_dir <- "Ergebnisse"
export_results(result = ergebnis, courses = kurse_import, students = schueler_import, output_dir = export_dir)

cat("\nWorkflow erfolgreich abgeschlossen. Sehen Sie sich die Dateien im Ordner 'Ergebnisse' an!\n")
