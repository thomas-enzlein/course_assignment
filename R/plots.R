#' Zufriedenheit nach Geschlecht visualisieren
#'
#' @param eval_res Resultat von evaluate_dashboard()
#' @param total_m Anzahl Jungen gesamt
#' @param total_w Anzahl Maedchen gesamt
#' @return Ein plotly Objekt
#' @importFrom plotly plot_ly add_trace layout
#' @export
plot_satisfaction_demographics <- function(eval_res, total_m, total_w) {
  # Helper von rlang oder lokal falls nicht vorhanden
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  df_plot <- data.frame(
    Category = factor(c("1. Wahl", "2. Wahl", "3. Wahl", "Warteliste"), 
                      levels = c("1. Wahl", "2. Wahl", "3. Wahl", "Warteliste")),
    M = c(eval_res$choice_1_m %||% 0, eval_res$choice_2_m %||% 0, eval_res$choice_3_m %||% 0, eval_res$unassigned_m %||% 0),
    W = c(eval_res$choice_1_w %||% 0, eval_res$choice_2_w %||% 0, eval_res$choice_3_w %||% 0, eval_res$unassigned_w %||% 0),
    Other = c(eval_res$choice_1_other %||% 0, eval_res$choice_2_other %||% 0, eval_res$choice_3_other %||% 0, eval_res$unassigned_other %||% 0)
  )
  
  other_total <- sum(df_plot$Other)
  df_plot$M_pct <- if (total_m > 0) round((df_plot$M / total_m) * 100, 1) else 0
  df_plot$W_pct <- if (total_w > 0) round((df_plot$W / total_w) * 100, 1) else 0
  df_plot$Other_pct <- if (other_total > 0) round((df_plot$Other / other_total) * 100, 1) else 0

  p <- plotly::plot_ly(df_plot, x = ~Category)
  
  if (total_m == 0 && total_w == 0) {
    p <- p |> plotly::add_trace(y = ~Other, type = "bar", name = "Gesamt", 
                marker = list(color = "#f39c12"),
                text = ~paste0(Other_pct, "%"), textposition = "auto",
                hovertext = ~paste0(Other, " von ", other_total, " (", Other_pct, "%) erhielten ", Category), 
                hoverinfo = "text")
  } else {
    if (total_m > 0) {
      p <- p |> plotly::add_trace(y = ~M, type = "bar", name = "Jungen (m)", 
                  marker = list(color = "#3498db"),
                  text = ~paste0(M_pct, "%"), textposition = "auto",
                  hovertext = ~paste0(M, " von ", total_m, " Jungen (", M_pct, "%) erhielten ", Category), 
                  hoverinfo = "text")
    }
    if (total_w > 0) {
      p <- p |> plotly::add_trace(y = ~W, type = "bar", name = "Mädchen (w)", 
                  marker = list(color = "#e74c3c"),
                  text = ~paste0(W_pct, "%"), textposition = "auto",
                  hovertext = ~paste0(W, " von ", total_w, " Mädchen (", W_pct, "%) erhielten ", Category), 
                  hoverinfo = "text")
    }
    if (other_total > 0) {
      p <- p |> plotly::add_trace(y = ~Other, type = "bar", name = "Sonstige / k.A.", 
                  marker = list(color = "#95a5a6"),
                  text = ~paste0(Other_pct, "%"), textposition = "auto",
                  hovertext = ~paste0(Other, " von ", other_total, " (", Other_pct, "%) erhielten ", Category), 
                  hoverinfo = "text")
    }
  }

  p |> plotly::layout(
    title = list(text = "Zufriedenheit nach Geschlecht<br><sup>Relative Anteile an der jeweiligen Gruppe</sup>"),
    barmode = "group",
    xaxis = list(title = ""),
    yaxis = list(title = "Anzahl Schüler")
  )
}

#' Kursauslastung visualisieren
#'
#' @param eval_res Resultat von evaluate_dashboard()
#' @return Ein plotly Objekt
#' @importFrom plotly plot_ly add_trace layout
#' @export
plot_course_occupancy <- function(eval_res) {
  stats <- eval_res$course_stats
  stats$free_capacity <- stats$max_capacity - stats$participants
  
  p2 <- plotly::plot_ly(stats, x = ~course_id)
  
  has_m <- any(stats$participants_m > 0)
  has_w <- any(stats$participants_w > 0)
  
  if (!has_m && !has_w) {
    p2 <- p2 |> plotly::add_trace(
      y = ~participants, type = "bar", name = "Belegt",
      marker = list(color = "#f39c12"),
      text = ~ paste0(
        "Kurs: ", course_name, " (", course_id, ")",
        "<br>Teilnehmer: ", participants, "/", max_capacity,
        "<br>Interessenten gesamt: ", total_interest,
        "<br>Status: ", ifelse(participants == 0, "Ausgefallen", "Aktiv")
      ),
      hoverinfo = "text",
      textposition = "none"
    )
  } else {
    if (has_m) {
      p2 <- p2 |> plotly::add_trace(
        y = ~participants_m, type = "bar", name = "Jungen (m)",
        marker = list(color = "#3498db"),
        text = ~ paste0(
          "Kurs: ", course_name, " (", course_id, ")",
          "<br>Jungen: ", participants_m,
          "<br>Mädchen: ", participants_w,
          "<br>Gesamt: ", participants, "/", max_capacity,
          "<br>Interessenten gesamt: ", total_interest,
          "<br>Status: ", ifelse(participants == 0, "Ausgefallen", "Aktiv")
        ),
        hoverinfo = "text",
        textposition = "none"
      )
    }
    if (has_w) {
      p2 <- p2 |> plotly::add_trace(
        y = ~participants_w, type = "bar", name = "Mädchen (w)",
        marker = list(color = "#e74c3c"),
        text = ~ paste0(
          "Kurs: ", course_name, " (", course_id, ")",
          "<br>Mädchen: ", participants_w,
          "<br>Jungen: ", participants_m,
          "<br>Gesamt: ", participants, "/", max_capacity,
          "<br>Interessenten gesamt: ", total_interest
        ),
        hoverinfo = "text",
        textposition = "none"
      )
    }
    if (any(stats$participants_other > 0)) {
      p2 <- p2 |> plotly::add_trace(
        y = ~participants_other, type = "bar", name = "Sonstige / k.A.",
        marker = list(color = "#95a5a6"),
        text = ~ paste0(
          "Kurs: ", course_name, " (", course_id, ")",
          "<br>Sonstige/k.A.: ", participants_other,
          "<br>Gesamt: ", participants, "/", max_capacity,
          "<br>Interessenten gesamt: ", total_interest
        ),
        hoverinfo = "text",
        textposition = "none"
      )
    }
  }
  
  p2 |> plotly::add_trace(
    y = ~free_capacity, type = "bar", name = "Frei",
    marker = list(color = "#bdc3c7"),
    text = ~ paste0(
      "Kurs: ", course_name, " (", course_id, ")",
      "<br>Frei: ", free_capacity,
      "<br>Mindestgröße: ", min_capacity,
      "<br>Interessenten gesamt: ", total_interest,
      ifelse(participants == 0, 
        paste0("<br>Grund für 0: ", ifelse(total_interest == 0, "Keine Wünsche", "Min-Cap nicht erreicht")),
        ""
      )
    ),
    hoverinfo = "text",
    textposition = "none"
  ) |>
    plotly::layout(
      title = list(text = "Kursauslastung & Nachfrage (m/w Split)"),
      barmode = "stack",
      xaxis = list(title = "Kurs ID"), yaxis = list(title = "Anzahl Schüler"),
      showlegend = TRUE
    )
}
