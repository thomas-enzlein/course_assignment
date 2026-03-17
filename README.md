# Kurswahl-Optimierung in R

Dieses R-Paket löst das klassische Problem der fairen Kurswahl an Schulen. Es verwendet **Mixed-Integer Linear Programming (MILP)** (mit `ompr` und `GLPK`), um Schüler basierend auf ihren Erst-, Zweit- und Drittwahlen so auf Kurse zu verteilen, dass die *Gesamtzufriedenheit der Schülerschaft global maximiert* wird.

Gleichzeitig werden harte Restriktionen wie Raumkapazitäten (Maximalgrenzen) und die Existenzsicherung von Kursen (Minimalgrenzen) mathematisch bewiesen durchgesetzt.

## Features
* **100% faires globales Optimum:** Kein "Wer zuerst kommt, mahlt zuerst" – das System findet die mathematisch beste Kombination aller Schüler gleichzeitig.
* **Strikes Kapazitätsmanagement:** Kurse haben harte Ober- und Untergrenzen.
* **Geschlechter-Balance & Diversität:** Optionale Constraints sorgen für eine faire Verteilung von Jungen und Mädchen sowie eine Durchmischung von Klassen.
* **Modernes Dashboard:** Interaktive Visualisierungen der Zufriedenheit und Kursauslastung (inkl. m/w Ratio und demografischen Splits).
* **Lebenserhaltungs-Flag:** Der Solver kann Kurse vor dem Ausfall retten, indem er Wünsche geschickt verteilt.
* **Excel-Unterstützung:** Direkter Import von und Export nach Excel (`.xlsx`).

---

## 1. Benötigte Datenstrukturen (Input)

Das Paket erwartet eine Excel-Datei mit zwei Reitern: **"Schüler"** und **"Kurse"**.

### Datei 1: Kursdaten (`Kurse`)
| Spaltenname | Beschreibung | Pflicht? |
| :--- | :--- | :---: |
| **`course_id`** | Eindeutiger Identifikator (z.B. `C01`). | **Ja** |
| **`max_capacity`** | Maximale Teilnehmerzahl. | **Ja** |
| **`min_capacity`** | Minimale Teilnehmerzahl. | **Ja** |
| **`course_name`** | Anzeigename für Grafiken. | Nein |

### Datei 2: Schülerdaten (`Schüler`)
| Spaltenname | Beschreibung | Pflicht? |
| :--- | :--- | :---: |
| **`student_id`** | Eindeutiger Bezeichner des Schülers. | **Ja** |
| **`first_choice`** | `course_id` der Erstwahl. | **Ja** |
| **`second_choice`**| `course_id` der Zweitwahl. | **Ja** |
| **`third_choice`** | `course_id` der Drittwahl. | **Ja** |
| **`gender`** | Geschlecht (`m`/`w`). Ermöglicht Balance-Constraint. | Nein |
| **`class`** | Klasse (oder `Klasse`). Ermöglicht Durchmischung. | Nein |

---

## 2. Nutzung (Workflow)

Am einfachsten nutzen Sie die integrierte grafische Oberfläche:

```r
library(kurszuweisung)
launch_gui()
```

Für die Nutzung im Skript:

```r
library(kurszuweisung)

# 1. Daten laden
daten <- import_data("meine_daten.xlsx")

# 2. Optimierung mit Gewichtung (0-100)
ergebnis <- optimize_courses(
  daten$students, 
  daten$courses, 
  balance_gender = 50, # 0 = aus, 100 = strikt
  balance_class = 30,
  time_limit_sec = 30
)

# 3. Auswerten & Exportieren
evaluate_dashboard(ergebnis, daten$students, daten$courses)
export_results(ergebnis, daten$courses, daten$students, "Ergebnisse")
```

## Fehlerbehebung & Performance
Der Solver (GLPK) garantiert bei genügend Zeit das globale Optimum. Bei sehr großen Problemen (über 500 Schüler) kann der Status `Time Limit` auftreten. Dies bedeutet, dass eine zulässige Lösung gefunden wurde, aber der mathematische Beweis für das absolute Optimum noch aussteht. Diese Ergebnisse sind in der Praxis fast immer sofort verwendbar.
