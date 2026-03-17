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
  
  # --- Centralized Evaluation Reactive ---
  # This avoids calling evaluate_dashboard 5+ times for every change
  eval_res <- reactive({
    req(rv$result, rv$students, rv$courses)
    kurszuweisung::evaluate_dashboard(rv$result, rv$students, rv$courses)
  })
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
    optimize_status = NULL,
    data_path = config$data_path
  )

  # --- shinyFiles Setup ---
  roots <- c(Home = normalizePath("~"), Root = "C:/", Current = getwd())
  shinyFiles::shinyFileChoose(input, "file_total", roots = roots, filetypes = c("xlsx"))

  # Helper to convert shinyFiles volume/path to absolute path
  parse_path <- function(file_input) {
    if (is.integer(file_input)) return(NULL)
    parseFilePaths(roots, file_input)$datapath
  }

  # Loading function
  load_all_data <- function(path) {
    if (is.null(path) || !file.exists(path)) return()
    tryCatch(
      {
        data <- kurszuweisung::import_data(path)
        rv$students <- data$students
        rv$courses <- data$courses
        showNotification("Excel-Daten erfolgreich geladen.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Fehler beim Laden der Excel-Datei:", e$message), type = "error")
        rv$students <- NULL
        rv$courses <- NULL
      }
    )
  }

  # --- Observers for File Selection ---
  observeEvent(input$file_total, {
    path <- parse_path(input$file_total)
    if (!is.null(path)) {
      rv$data_path <- path
      load_all_data(path)
    }
  })

  # --- Reload Logic ---
  observeEvent(input$btn_reload, {
    if (!is.null(rv$data_path)) {
      load_all_data(rv$data_path)
    }
  })

  # --- Initial Load ---
  observe({
    if (!is.null(rv$data_path) && is.null(rv$students)) {
      load_all_data(rv$data_path)
    }
  })

  # --- Status Outputs for Files ---
  output$status_total <- renderUI({
    if (!is.null(rv$students) && !is.null(rv$courses)) {
      span(icon("check-circle"), " Gesamt-Excel geladen (Schüler + Kurse)", style = "color: #27ae60; font-size: 0.8rem;")
    } else {
      span(icon("exclamation-triangle"), " Bitte Excel-Datei wählen", style = "color: #e67e22; font-size: 0.8rem;")
    }
  })

  # --- Initial Parameter Setting ---
  observe({
    req(config)
    updateSliderInput(session, "time_limit", value = config$time_limit)
    updateCheckboxInput(session, "enforce_survival", value = config$enforce_survival)
    
    # New balancing parameters (with defaults for old configs)
    if (!is.null(config$use_balance_gender)) updateCheckboxInput(session, "use_balance_gender", value = config$use_balance_gender)
    if (!is.null(config$use_balance_class)) updateCheckboxInput(session, "use_balance_class", value = config$use_balance_class)
    if (!is.null(config$weight_gender)) updateSliderInput(session, "weight_gender", value = config$weight_gender)
    if (!is.null(config$weight_class)) updateSliderInput(session, "weight_class", value = config$weight_class)
  })

  # --- Auto-Save Config ---
  observe({
    save_config(
      rv$data_path,
      input$time_limit,
      input$enforce_survival,
      input$use_balance_gender,
      input$use_balance_class,
      input$weight_gender,
      input$weight_class
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
    req(rv$data_path)

    # Automatischer Reload vor dem Start, um sicherzugehen dass wir die neuesten Daten haben
    load_all_data(rv$data_path)
    
    req(rv$students, rv$courses)

    withProgress(message = "Optimierung läuft...", value = 0.5, {
      res <- tryCatch(
        {
          kurszuweisung::optimize_courses(
            students = rv$students,
            courses = rv$courses,
            enforce_survival = input$enforce_survival,
            balance_gender = if (isTRUE(input$use_balance_gender)) input$weight_gender else 0,
            balance_class = if (isTRUE(input$use_balance_class)) input$weight_class else 0,
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
        rv$data_path,
        input$time_limit,
        input$enforce_survival
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

  output$plot_satisfaction <- renderPlotly({
    m_total <- sum(rv$students$gender == "m", na.rm = TRUE)
    w_total <- sum(rv$students$gender == "w", na.rm = TRUE)
    kurszuweisung::plot_satisfaction_demographics(eval_res(), m_total, w_total)
  })

  output$plot_course_usage <- renderPlotly({
    kurszuweisung::plot_course_occupancy(eval_res())
  })

  # --- Outputs: Tables ---
  output$table_assignments <- renderDT({
    req(rv$result, rv$students, rv$courses)
    assignments <- rv$result$assignments

    # Merge student info
    df <- merge(assignments, rv$students, by = "student_id", all.x = TRUE)

    # Helper for human names
    get_name <- function(id) {
      if (is.na(id) || id == "") return("-")
      name <- rv$courses$course_name[rv$courses$course_id == id]
      if (length(name) == 1) paste0(name, " (", id, ")") else id
    }

    # Choice calculation
    df$Status <- ifelse(df$course_id == df$first_choice, "1. Wahl",
                  ifelse(df$course_id == df$second_choice, "2. Wahl",
                    ifelse(df$course_id == df$third_choice, "3. Wahl", "Sonderzuweisung")))

    df$Zuweisung <- vapply(df$course_id, get_name, character(1))
    df$W1 <- vapply(df$first_choice, get_name, character(1))
    df$W2 <- vapply(df$second_choice, get_name, character(1))
    df$W3 <- vapply(df$third_choice, get_name, character(1))

    # Select columns dynamically
    cols <- c("student_id", "student_name", "gender")
    if ("class" %in% names(df)) cols <- c(cols, "class")
    else if ("Klasse" %in% names(df)) cols <- c(cols, "Klasse")
    
    cols <- c(cols, "Status", "Zuweisung", "W1", "W2", "W3")
    display_df <- df[, cols]
    
    # Rename for UI
    col_names <- c("ID", "Name", "Geschl.", "Klasse", "Status", "Kurs", "1. Wunsch", "2. Wunsch", "3. Wunsch")
    colnames(display_df) <- col_names[c(TRUE, TRUE, TRUE, ("class" %in% names(df) || "Klasse" %in% names(df)), TRUE, TRUE, TRUE, TRUE, TRUE)]

    display_df |> datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$table_courses <- renderDT({
    # Use reactive results
    res <- eval_res()
    display_df <- res$course_stats[, c("course_name", "course_id", "participants", "participants_m", "participants_w", "max_capacity", "min_capacity", "total_interest")]
    
    # Calculate raw ratio string
    display_df$ratio <- paste0(display_df$participants_m, "m / ", display_df$participants_w, "w")
    
    # Final column selection for UI
    display_df <- display_df[, c("course_name", "course_id", "participants", "ratio", "max_capacity", "min_capacity", "total_interest")]
    colnames(display_df) <- c("Kursname", "Kurs-ID", "Teilnehmer (Gesamt)", "m/w Ratio", "Max.", "Min.", "Interesse")

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

  # Permanent Speichern in Excel-Datei
  observeEvent(input$btn_save_courses, {
    req(rv$courses, rv$data_path, rv$students)
    tryCatch({
      # Wir schreiben beide Reiter zurück, um die Datei konsistent zu halten
      sheets <- list(
        "Schüler" = rv$students,
        "Kurse" = rv$courses
      )
      writexl::write_xlsx(sheets, path = rv$data_path)
      showNotification("Änderungen permanent in der Excel-Datei gespeichert!", type = "message")
    }, error = function(e) {
      showNotification(paste("Speichern fehlgeschlagen:", e$message), type = "error")
    })
  })

  output$table_unassigned <- renderDT({
    # Use reactive results
    df <- eval_res()$rest_students
    
    # Helper for human names
    map_course <- function(id) {
       if (is.na(id) || id == "") return("-")
       name <- rv$courses$course_name[rv$courses$course_id == id]
       if (length(name) == 1) paste0(name, " (", id, ")") else id
    }

    df$c1 <- vapply(df$first_choice, map_course, character(1))
    df$c2 <- vapply(df$second_choice, map_course, character(1))
    df$c3 <- vapply(df$third_choice, map_course, character(1))

    # Basic columns
    cols <- c("student_id")
    if ("student_name" %in% names(df)) cols <- c(cols, "student_name")
    if ("gender" %in% names(df)) cols <- c(cols, "gender")
    if ("class" %in% names(df)) cols <- c(cols, "class")
    else if ("Klasse" %in% names(df)) cols <- c(cols, "Klasse")
    
    # Satisfaction data
    req_cols <- c("c1", "reason_1", "c2", "reason_2", "c3", "reason_3")
    cols <- c(cols, intersect(req_cols, names(df)))

    display_df <- df[, cols, drop = FALSE]
    
    # Clean names
    header <- c("ID", "Name", "m/w", "Klasse", "1. Wunsch", "Grund 1", "2. Wunsch", "Grund 2", "3. Wunsch", "Grund 3")
    colnames(display_df) <- header[1:ncol(display_df)]

    display_df |> datatable(rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  # --- Excel Export Handler ---
  output$export_excel <- downloadHandler(
    filename = function() {
      paste("kurszuweisung-ergebnisse-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(rv$result, rv$students, rv$courses)
      
      # 1. Zuweisungen aufbereiten (Logik analog zu export_results)
      assignments <- rv$result$assignments
      cols_students <- intersect(names(rv$students), c("student_id", "student_name", "class", "Klasse", "first_choice", "second_choice", "third_choice"))
      assignments <- merge(assignments, rv$students[, cols_students], by = "student_id", all.x = TRUE)
      
      assignments$choice_num <- "Zufall/Reste"
      assignments$choice_num[assignments$course_id == assignments$first_choice] <- "1. Wahl"
      assignments$choice_num[assignments$course_id == assignments$second_choice] <- "2. Wahl"
      assignments$choice_num[assignments$course_id == assignments$third_choice] <- "3. Wahl"
      
      if ("course_name" %in% names(rv$courses)) {
        assignments <- merge(assignments, rv$courses[, c("course_id", "course_name")], by = "course_id", all.x = TRUE)
      }
      
      final_assign_cols <- c("student_id")
      if ("student_name" %in% names(assignments)) final_assign_cols <- c(final_assign_cols, "student_name")
      if ("class" %in% names(assignments)) {
        final_assign_cols <- c(final_assign_cols, "class")
      } else if ("Klasse" %in% names(assignments)) {
        final_assign_cols <- c(final_assign_cols, "Klasse")
      }
      final_assign_cols <- c(final_assign_cols, "course_id")
      if ("course_name" %in% names(assignments)) final_assign_cols <- c(final_assign_cols, "course_name")
      final_assign_cols <- c(final_assign_cols, "choice_num")
      
      assignments_df <- assignments[, final_assign_cols]
      colnames(assignments_df) <- c("Schüler-ID", "Name", "Klasse", "Kurs-ID", "Kursname", "Wunsch")[1:ncol(assignments_df)]
      
      # 2. Kurs-Statistik aufbereiten
      eval_res <- kurszuweisung::evaluate_dashboard(rv$result, rv$students, rv$courses)
      stats_df <- eval_res$course_stats[, c("course_name", "course_id", "participants", "max_capacity", "min_capacity", "total_interest")]
      colnames(stats_df) <- c("Kursname", "Kurs-ID", "Teilnehmer", "Max.", "Min.", "Interesse")
      
      # 3. In Excel schreiben
      sheets <- list("Zuweisungen" = assignments_df, "Kurs-Statistik" = stats_df)
      writexl::write_xlsx(sheets, path = file)
    }
  )

  # --- Template Export Handler ---
  output$export_templates <- downloadHandler(
    filename = function() {
      paste("kurszuweisung-vorlage-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Direkt frische Demo-Daten in die Excel-Datei generieren
      kurszuweisung::generate_demo_excel(file)
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
