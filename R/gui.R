#' Kurszuweisung GUI starten
#'
#' Startet eine interaktive Web-Oberfläche (Shiny) zur Optimierung der Kurswahlen.
#' Die GUI erlaubt den Datei-Upload, die Parametrisierung und die Visualisierung
#' der Ergebnisse.
#'
#' @param port Der Port, auf dem die App laufen soll (Standard: Zufällig).
#' @param launch.browser Logisch, ob die App im Browser geöffnet werden soll (Standard: TRUE).
#' @export
#' @importFrom shiny runApp
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom bslib bs_theme
#' @importFrom DT datatable
#' @importFrom rmarkdown render
#' @importFrom knitr knit
#' @importFrom jsonlite fromJSON
launch_gui <- function(port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", interactive())) {
  app_dir <- system.file("shiny-app", package = "kurszuweisung")

  if (app_dir == "") {
    stop("Die Shiny-App konnte nicht gefunden werden. Bitte installiere das Paket neu.", call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, port = port, launch.browser = launch.browser)
}
