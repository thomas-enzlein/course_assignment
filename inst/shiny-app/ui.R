#' Shiny UI for Course Assignment App
#'
#' Modern bslib layout with sidebar and card-based content.

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(plotly)

ui <- page_sidebar(
  title = "Kurszuweisung Optimierer",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#2c3e50"
  ) |>
    bs_add_rules("body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; }"),
  useShinyjs(),
  sidebar = sidebar(
    title = "Einstellungen",
      # --- Inputs: Files ---
      card(
        card_header("Dateneingabe"),
        div(
          class = "d-flex align-items-center mb-3",
          shinyFiles::shinyFilesButton(
            "file_students", "Schüler wählen...",
            title = "Schüler-CSV auswählen", multiple = FALSE,
            buttonType = "default", class = "btn-sm me-2"
          ),
          uiOutput("status_students")
        ),
        div(
          class = "d-flex align-items-center mb-3",
          shinyFiles::shinyFilesButton(
            "file_courses", "Kurse wählen...",
            title = "Kurs-CSV auswählen", multiple = FALSE,
            buttonType = "default", class = "btn-sm me-2"
          ),
          uiOutput("status_courses")
        ),
        actionButton(
          "btn_reload", "Daten neu laden",
          icon = icon("refresh"), class = "btn-outline-secondary btn-sm w-100"
        ),
        helpText("Wähle die CSV-Dateien direkt auf deiner Festplatte aus.")
      ),
    card(
      card_header("Parameter"),
      sliderInput("time_limit", "Zeitlimit (Sekunden)", min = 5, max = 120, value = 15),
      checkboxInput("enforce_survival", "Kurse 'retten' (Enforce Survival)", value = FALSE)
    ),
    input_task_button("run_opt", "Optimierung starten", icon = icon("rocket")),
    br(),
    downloadButton("export_results", "CSV Daten (ZIP)", class = "btn-secondary w-100 mb-2"),
    downloadButton("export_pdf", "Bericht (PDF)", class = "btn-info w-100", icon = icon("file-pdf"))
  ),

  # 1. Row: Status & Metriken (Kompakt am oberen Rand)
  layout_column_wrap(
    width = 1 / 4,
    fill = FALSE, # Wichtig: Verhindert, dass die Boxen vertikal wachsen
    value_box(
      title = "Status",
      value = textOutput("opt_status_friendly"),
      theme = "primary"
    ),
    value_box(
      title = "1. Wahl Quote",
      value = textOutput("choice_1_pct"),
      theme = "success"
    ),
    value_box(
      title = "Nicht zugewiesen",
      value = textOutput("unassigned_count"),
      theme = "danger"
    ),
    value_box(
      title = "Score",
      value = textOutput("total_score"),
      theme = "info"
    )
  ),

  # 2. Row: Ergebnisse (Füllt den restlichen Platz)
  navset_card_pill(
    title = "Ergebnisse",
    nav_panel(
      "Übersicht",
      card(
        card_header("Zufriedenheit & Kurs-Status"),
        layout_column_wrap(
          width = 1 / 2,
          plotlyOutput("plot_satisfaction"),
          plotlyOutput("plot_course_usage")
        )
      )
    ),
    nav_panel(
      "Zuweisungs-Liste",
      card(
        card_header("Detaillierte Schüler-Zuweisung"),
        DTOutput("table_assignments")
      )
    ),
    nav_panel(
      "Kurs-Details",
      card(
        card_header("Status aller Kurse (Ergebnisse)"),
        DTOutput("table_courses")
      )
    ),
    nav_panel(
      "Konfiguration",
      card(
        card_header("Kurs-Kapazitäten anpassen (Input)"),
        DTOutput("table_courses_config"),
        actionButton(
          "btn_save_courses", "Änderungen permanent in CSV speichern", 
          icon = icon("save"), class = "btn-success mt-2 w-100"
        ),
        helpText("Änderungen in der Tabelle oben werden sofort für die nächste Optimierung übernommen. Nutze den Button, um sie dauerhaft in der Datei zu sichern.")
      )
    ),
    nav_panel(
      "Warteliste",
      card(
        card_header("Schüler ohne Zuweisung"),
        DTOutput("table_unassigned")
      )
    )
  )
)
