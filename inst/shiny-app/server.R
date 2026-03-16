#' Shiny Server for Course Assignment App
#'
#' Handles reactive data, file validation, optimization trigger,
#' and rendering of results.

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(kurszuweisung)
library(shinyFiles) # Added shinyFiles library

# Source persistence logic (local to the app)
source("persistence.R")

server <- function(input, output, session) {
  # --- Persistence: Load initial config ---
  config <- load_config()

  # Initial reactive values
  # --- Pandoc Setup ---
  # Falls Pandoc nicht gefunden wird, versuchen wir es ueber das 'pandoc' R-Paket zu lokalisieren
  if (!rmarkdown::pandoc_available()) {
    try({
      p_path <- pandoc::pandoc_locate()
      if (!is.null(p_path)) {
        Sys.setenv(RSTUDIO_PANDOC = p_path)
      }
    }, silent = TRUE)
  }

  rv <- reactiveValues(
    students = NULL,
    courses = NULL,
    result = NULL,
    optimize_status = NULL, # Added optimize_status
    students_path = config$students_path,
    courses_path = config$courses_path
  )

  # --- shinyFiles Setup ---
  roots <- c(Home = normalizePath("~"), Root = "C:/", Current = getwd())
  shinyFiles::shinyFileChoose(input, "file_students", roots = roots, filetypes = c("csv"))
  shinyFiles::shinyFileChoose(input, "file_courses", roots = roots, filetypes = c("csv"))

  # Helper to convert shinyFiles volume/path to absolute path
  parse_path <- function(file_input) {
    if (is.integer(file_input)) return(NULL)
    parseFilePaths(roots, file_input)$datapath
  }

  # Loading function
  load_data <- function(path, type) {
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(
      {
        read.csv(path, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Fehler beim Laden von", type, ":", e$message), type = "error")
        NULL
      }
    )
  }

  # --- Observers for File Selection ---
  observeEvent(input$file_students, {
    path <- parse_path(input$file_students)
    if (!is.null(path)) {
      rv$students_path <- path
      rv$students <- load_data(path, "Schüler")
    }
  })

  observeEvent(input$file_courses, {
    path <- parse_path(input$file_courses)
    if (!is.null(path)) {
      rv$courses_path <- path
      rv$courses <- load_data(path, "Kurse")
    }
  })

  # --- Reload Logic ---
  observeEvent(input$btn_reload, {
    if (!is.null(rv$students_path)) rv$students <- load_data(rv$students_path, "Schüler")
    if (!is.null(rv$courses_path)) rv$courses <- load_data(rv$courses_path, "Kurse")
    if (!is.null(rv$students) || !is.null(rv$courses)) {
      showNotification("Daten erfolgreich neu eingelesen.", type = "message")
    }
  })

  # --- Initial Load ---
  observe({
    if (!is.null(rv$students_path) && is.null(rv$students)) {
      rv$students <- load_data(rv$students_path, "Schüler")
    }
    if (!is.null(rv$courses_path) && is.null(rv$courses)) {
      rv$courses <- load_data(rv$courses_path, "Kurse")
    }
  })

  # --- Status Outputs for Files ---
  output$status_students <- renderUI({
    if (!is.null(rv$students)) {
      span(icon("check-circle"), " Daten geladen", style = "color: #27ae60; font-size: 0.8rem;")
    } else {
      span(icon("exclamation-triangle"), " Bitte Datei wählen", style = "color: #e67e22; font-size: 0.8rem;")
    }
  })

  output$status_courses <- renderUI({
    if (!is.null(rv$courses)) {
      span(icon("check-circle"), " Daten geladen", style = "color: #27ae60; font-size: 0.8rem;")
    } else {
      span(icon("exclamation-triangle"), " Bitte Datei wählen", style = "color: #e67e22; font-size: 0.8rem;")
    }
  })

  # --- Initial Parameter Setting ---
  observe({
    req(config)
    updateSliderInput(session, "time_limit", value = config$time_limit)
    updateCheckboxInput(session, "enforce_survival", value = config$enforce_survival)
  })

  # --- Auto-Save Config ---
  observe({
    save_config(
      rv$students_path,
      rv$courses_path,
      input$time_limit,
      input$enforce_survival
    )
  })

  # --- UX Validation: Enable/Disable Button ---
  observe({
    can_run <- !is.null(rv$students) && !is.null(rv$courses)
    if (can_run) {
      enable("run_opt")
    } else {
      disable("run_opt")
    }
  })

  # --- Run Optimization ---
  observeEvent(input$run_opt, {
    req(rv$students, rv$courses)

    withProgress(message = "Optimierung läuft...", value = 0.5, {
      res <- tryCatch(
        {
          kurszuweisung::optimize_courses(
            students = rv$students,
            courses = rv$courses,
            enforce_survival = input$enforce_survival,
            time_limit_sec = input$time_limit
          )
        },
        error = function(e) {
          showNotification(paste("Fehler bei der Optimierung:", e$message), type = "error")
          NULL
        }
      )

      rv$result <- res

      # Save config on successful run
      save_config(
        students_path = rv$students_path,
        courses_path = rv$courses_path,
        time_limit = input$time_limit,
        enforce_survival = input$enforce_survival
      )
    })

    if (!is.null(rv$result)) {
      showNotification("Optimierung erfolgreich abgeschlossen!", type = "message")
    }
  })

  # --- Outputs: Value Boxes ---
  output$opt_status_friendly <- renderText({
    req(rv$result)
    status <- tolower(rv$result$status)
    if (status %in% c("optimal", "success")) {
      "Optimal gelöst"
    } else if (grepl("limit", status) || status == "error") {
      # "error" in GLPK/ROI often means time limit reached but solution available
      "Teillösung (Zeitlimit)"
    } else if (status == "infeasible") {
      "Keine Lösung möglich"
    } else {
      status
    }
  })

  output$choice_1_pct <- renderText({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(rv$result, rv$students, rv$courses)
    sprintf("%.1f %%", (eval_res$choice_1 / nrow(rv$students)) * 100)
  })

  output$unassigned_count <- renderText({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(rv$result, rv$students, rv$courses)
    eval_res$unassigned
  })

  output$total_score <- renderText({
    req(rv$result)
    sprintf("%.2f", rv$result$objective_value)
  })

  # --- Outputs: Plots ---
  output$plot_satisfaction <- renderPlotly({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(rv$result, rv$students, rv$courses)

    df_plot <- data.frame(
      Category = c("1. Wahl", "2. Wahl", "3. Wahl", "Nicht zugewiesen"),
      Count = c(eval_res$choice_1, eval_res$choice_2, eval_res$choice_3, eval_res$unassigned)
    )

    plot_ly(df_plot,
      labels = ~Category, values = ~Count, type = "pie",
      marker = list(colors = c("#27ae60", "#f1c40f", "#e67e22", "#c0392b"))
    ) |>
      layout(title = list(text = "Verteilung der Wünsche"))
  })

  output$plot_course_usage <- renderPlotly({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(
      rv$result, rv$students, rv$courses
    )
    stats <- eval_res$course_stats
    stats$free_capacity <- stats$max_capacity - stats$participants

    plot_ly(stats,
      x = ~course_id, y = ~participants, type = "bar", name = "Belegt",
      marker = list(color = "#f39c12"), # Orange
      text = ~ paste0(
        "Kurs: ", course_name, 
        "<br>Teilnehmer: ", participants, "/", max_capacity,
        "<br>Interessenten gesamt: ", total_interest,
        "<br>Status: ", ifelse(participants == 0, "Ausgefallen", "Aktiv")
      ),
      hoverinfo = "text",
      textposition = "none"
    ) |>
      add_trace(
        y = ~free_capacity, name = "Frei",
        marker = list(color = "#bdc3c7"), # Gray
        text = ~ paste0(
          "Kurs: ", course_name, 
          "<br>Frei: ", free_capacity,
          "<br>Mindestgröße: ", min_capacity,
          "<br>Interessenten: ", total_interest,
          ifelse(participants == 0, 
            paste0("<br>Grund für 0: ", ifelse(total_interest == 0, "Keine Wünsche", "Min-Cap nicht erreicht")),
            ""
          )
        ),
        hoverinfo = "text",
        textposition = "none"
      ) |>
      layout(
        title = list(text = "Kursauslastung & Nachfrage"),
        barmode = "stack",
        xaxis = list(title = "Kurs ID"), yaxis = list(title = "Anzahl"),
        showlegend = TRUE
      )
  })

  # --- Outputs: Tables ---
  output$table_assignments <- renderDT({
    req(rv$result, rv$students, rv$courses)
    assignments <- rv$result$assignments

    # Merge student info (including choices)
    df <- merge(
      assignments, rv$students,
      by = "student_id"
    )

    # Helper for human names
    get_name <- function(id) {
      name <- rv$courses$course_name[rv$courses$course_id == id]
      if (length(name) == 1) paste0(name, " (", id, ")") else id
    }

    # Choice calculation & mapping
    df$Wunsch <- vapply(seq_len(nrow(df)), function(i) {
      actual <- df$course_id[i]
      if (actual == df$first_choice[i]) "1. Wunsch"
      else if (actual == df$second_choice[i]) "2. Wunsch"
      else if (actual == df$third_choice[i]) "3. Wunsch"
      else "Kein Wunsch (Auffüll-Zuweisung)"
    }, character(1))

    df$Zuweisung <- vapply(df$course_id, get_name, character(1))
    df$W1 <- vapply(df$first_choice, get_name, character(1))
    df$W2 <- vapply(df$second_choice, get_name, character(1))
    df$W3 <- vapply(df$third_choice, get_name, character(1))

    # Select columns
    display_df <- df[, c("student_id", "student_name", "Wunsch", "Zuweisung", "W1", "W2", "W3")]
    colnames(display_df) <- c("Schüler-ID", "Name", "Status", "Erhalten", "1. Wunsch", "2. Wunsch", "3. Wunsch")

    display_df |> datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$table_courses <- renderDT({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(
      rv$result, rv$students, rv$courses
    )
    # Order: Name first, then ID
    display_df <- eval_res$course_stats[, c("course_name", "course_id", "participants", "max_capacity", "min_capacity", "total_interest")]
    colnames(display_df) <- c("Kursname", "Kurs-ID", "Teilnehmer", "Max.", "Min.", "Interesse (alle Wünsche)")

    display_df |> datatable(rownames = FALSE)
  })

  # --- COURSE CONFIG (EDITABLE) ---
  output$table_courses_config <- renderDT({
    req(rv$courses)
    # Zeige nur relevante Spalten zum Editieren (Kategorie ist nur für Testdaten relevant)
    cols_wanted <- c("course_id", "course_name", "min_capacity", "max_capacity")
    cols <- intersect(names(rv$courses), cols_wanted)
    
    df <- rv$courses[, cols]
    
    # Dynamische Header (falls course_name fehlt)
    friendly_names <- c(
      "course_id" = "Kurs-ID", 
      "course_name" = "Kursname", 
      "min_capacity" = "Min-Cap", 
      "max_capacity" = "Max-Cap"
    )
    colnames(df) <- friendly_names[cols]
    
    # Welche Spalten sind editierbar? (Nur Min-Cap und Max-Cap)
    # Beachte: JS-Index ist 0-basiert.
    editable_cols <- which(cols %in% c("min_capacity", "max_capacity")) - 1
    disabled_cols <- setdiff(seq_along(cols) - 1, editable_cols)
    
    datatable(
      df, 
      selection = "none", 
      editable = list(target = "cell", disable = list(columns = disabled_cols)),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # Proxy für Updates
  proxy_courses <- dataTableProxy("table_courses_config")

  observeEvent(input$table_courses_config_cell_edit, {
    info <- input$table_courses_config_cell_edit
    
    # Dynamische Zuordnung der Spalte basierend auf der Anzeige
    cols_wanted <- c("course_id", "course_name", "min_capacity", "max_capacity")
    cols_shown <- intersect(names(rv$courses), cols_wanted)
    
    col_name <- cols_shown[info$col + 1]
    row_idx <- info$row
    val <- as.integer(info$value)
    
    if (is.na(val) || val < 0) {
      showNotification("Ungültiger Wert. Bitte eine positive Zahl eingeben.", type = "error")
      return()
    }

    if (col_name %in% c("min_capacity", "max_capacity")) {
      res <- tryCatch({
        kurszuweisung::update_course_capacity(rv$courses, row_idx, col_name, val)
      }, error = function(e) {
        showNotification(paste("Fehler bei der Aktualisierung:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(res)) {
        rv$courses <- res
        showNotification(sprintf("Vorgabe für '%s' aktualisiert.", rv$courses$course_name[row_idx]), type = "message")
      }
    }
  })

  # Permanent Speichern
  observeEvent(input$btn_save_courses, {
    req(rv$courses, rv$courses_path)
    tryCatch({
      write.csv(rv$courses, rv$courses_path, row.names = FALSE)
      showNotification("Änderungen permanent in der CSV gespeichert!", type = "message")
    }, error = function(e) {
      showNotification(paste("Speichern fehlgeschlagen:", e$message), type = "error")
    })
  })

  output$table_unassigned <- renderDT({
    req(rv$result, rv$students, rv$courses)
    eval_res <- kurszuweisung::evaluate_dashboard(
      rv$result, rv$students, rv$courses
    )
    df <- eval_res$rest_students

    # Helper function to map ID to "Name (ID)"
    map_course <- function(id) {
      if (is.na(id) || id == "") return(id)
      name <- rv$courses$course_name[rv$courses$course_id == id]
      if (length(name) == 1) paste0(name, " (", id, ")") else id
    }

    df$c1 <- vapply(df$first_choice, map_course, character(1))
    df$c2 <- vapply(df$second_choice, map_course, character(1))
    df$c3 <- vapply(df$third_choice, map_course, character(1))

    display_df <- df[, c("student_id", "student_name", "c1", "reason_1", "c2", "reason_2", "c3", "reason_3")]
    colnames(display_df) <- c("ID", "Name", "1. Wunsch", "Grund 1", "2. Wunsch", "Grund 2", "3. Wunsch", "Grund 3")

    display_df |> datatable(rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  # --- Download Handler ---
  output$export_results <- downloadHandler(
    filename = function() {
      paste("kurszuweisung-data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_dir <- tempdir()
      kurszuweisung::export_results(rv$result, rv$courses, rv$students, output_dir = temp_dir)

      # Zipping the files
      files_to_zip <- c(
        file.path(temp_dir, "zuweisungen_schueler.csv"),
        file.path(temp_dir, "kurs_zusammenfassung.csv")
      )
      zip(file, files_to_zip, extras = "-j")
    }
  )

  # --- PDF Report Handler ---
  output$export_pdf <- downloadHandler(
    filename = function() {
      paste("kurszuweisung-bericht-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(rv$result, rv$students, rv$courses)
      
      report_path <- system.file("reports/result_report.Rmd", package = "kurszuweisung")
      if (report_path == "") {
        report_path <- "../reports/result_report.Rmd"
      }
      
      showNotification("PDF-Bericht wird gerendert...", type = "message")
      
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy(report_path, temp_report, overwrite = TRUE)
      
      rmarkdown::render(
        temp_report, 
        output_file = file,
        params = list(
          result = rv$result,
          students = rv$students,
          courses = rv$courses
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}
