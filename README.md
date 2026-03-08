# Kurswahl-Optimierung in R

Dieses R-Paket löst das klassische Problem der fairen Kurswahl an Schulen. Es verwendet **Mixed-Integer Linear Programming (MILP)** (mit `ompr` und `GLPK`), um Schüler basierend auf ihren Erst-, Zweit- und Drittwahlen so auf Kurse zu verteilen, dass die *Gesamtzufriedenheit der Schülerschaft global maximiert* wird.

Gleichzeitig werden harte Restriktionen wie Raumkapazitäten (Maximalgrenzen) und die Existenzsicherung von Kursen (Minimalgrenzen) mathematisch bewiesen durchgesetzt.

## Features
* **100% faires globales Optimum:** Kein "Wer zuerst kommt, mahlt zuerst" – das System findet die mathematisch beste Kombination aller Schüler gleichzeitig.
* **Strikes Kapazitätsmanagement:** Kurse können harte Ober- und Untergrenzen haben.
* **Lebenserhaltungs-Flag (optional):** Der Solver kann so konfiguriert werden, dass er versucht, so viele Kurse wie möglich vor dem Ausfall (aufgrund zu weniger Teilnehmer) zu retten, anstatt nur stur Erstwahlen zu priorisieren.
* **Einfacher Ex- und Import:** Lesen Sie einfach CSV-Tabellen aus Excel ein und exportieren Sie fertige, menschenlesbare Zuweisungs-Daten.

---

## 1. Benötigte Datenstrukturen (Input)

Für den Import der Daten aus (z.B. aus Excel exportierten) CSV-Dateien benötigt das Paket exakt zwei Dateien mit einer strikten Struktur. Nur die Spaltennamen in der Kopfzeile müssen exakt passen, die Reihenfolge der Spalten ist irrelevant.

### Datei 1: Kursdaten (`kurse.csv`)
Definiert die verfügbaren Kurse, ihre Bezeichner und ihre physischen Grenzwerte.

| Spaltenname | Beschreibung | Pflicht? | Typ |
| :--- | :--- | :---: | :--- |
| **`course_id`** | Ein absoluter, eindeutiger Identifikator für den Kurs (z.B. `C001`, `Info_12`). | **Ja** | Text |
| **`min_capacity`** | Die absolute Untergrenze, ab der ein Kurs stattfinden darf (z.B. `4`). | **Ja** | Zahl |
| **`max_capacity`** | Die absolute Obergrenze (z.B. `15` für Fachräume, `30` generell). | **Ja** | Zahl |
| **`course_name`** | Gelesen freundlicher Name für Export & Dashboard. | Nein | Text |

### Datei 2: Schülerdaten (`schueler.csv`)
Enthält die eigentlichen Wahlen der SuS.

| Spaltenname | Beschreibung | Pflicht? | Typ |
| :--- | :--- | :---: | :--- |
| **`student_id`** | Ein eindeutiger Bezeichner des Schülers (z.B. `S001`, `M_Mustermann`). | **Ja** | Text |
| **`first_choice`** | Die `course_id` der Erstwahl. **Muss in Kurs-Datei existieren!** | **Ja** | Text |
| **`second_choice`**| Die `course_id` der Zweitwahl. | **Ja** | Text |
| **`third_choice`** | Die `course_id` der Drittwahl. | **Ja** | Text |
| **`tie_breaker`**  | Eine winzige Zufallszahl (z.B. 0.05) zur Auflösung bei absolutem Gleichstand. *Wird automatisch generiert, falls abwesend.* | Nein | Zahl |

---

## 2. Nutzung (Workflow mit Beispieldaten)

Das Paket liefert realistische Beispieldaten direkt mit. Sie können diese nutzen, um das System sofort auszuprobieren:

```r
library(kurszuweisung)

# 1. Beispieldaten-Pfade aus dem installierten Paket abrufen
students_file <- example_data_path("students")
courses_file <- example_data_path("courses")

# 2. Daten sicher importieren und auf Fehler validieren
daten <- import_data(students_file, courses_file)
schueler <- daten$students
kurse <- daten$courses

# 3. Optimierung starten 
# (Das Limit in Sekunden ist anpassbar; 45s sind oft gut für große Schulen)
ergebnis <- optimize_courses(schueler, kurse, 
                             enforce_survival = FALSE, 
                             time_limit_sec = 45)

# 4. Ergebnisse im direkten R-Dashboard betrachten
evaluate_dashboard(ergebnis, schueler, kurse)

# 5. Saubere Resultate als CSV für Excel exportieren
export_results(result = ergebnis, courses = kurse, students = schueler, output_dir = "Ergebnisse")
```

## Über den Solver (GLPK / Time-Limit)
Das `enforce_survival = TRUE` Flag zwingt den Solver dazu, unbeliebte Kurse gewaltsam aufzufüllen, was das mathematische Problem drastisch komplexer macht. 
Der Status `Abbruch durch Zeitlimit (Valide Zwischenlösung gefunden)` ist **kein Fehler**, sondern bedeutet lediglich, dass der Solver das globale (theoretische) Optimum nicht innerhalb der Zeit (z.B. 45s) mathematisch zu 100% lupenrein beweisen konnte, er aber exzellente Ergebnisse vorliegen hat. Sie können die exportierten Dateien problemlos verwenden!
