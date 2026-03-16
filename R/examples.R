#' Beispieldaten-Pfade abrufen
#'
#' Diese Funktion liefert die absoluten Pfade zu den mitgelieferten Beispieldaten
#' (Schueler und Kurse) des Pakets. Dies ist nuetzlich, um die Funktionen ohne eigene Daten auszuprobieren.
#'
#' @param type Charakter. Entweder \code{"students"} oder \code{"courses"}.
#' @return Einen Dateipfad (Character).
#' @export
#' @examples
#' \dontrun{
#' # 1. Daten generieren (Stress-Szenario 3.1)
#' courses <- generate_courses()
#' students <- generate_students(300, courses = courses)
#' }
example_data_path <- function(type = c("students", "courses")) {
  type <- match.arg(type)
  file_name <- switch(type,
    students = "schueler.csv",
    courses = "kurse.csv")

  # system.file sucht im inst/extdata Ordner des installierten Paketes
  path <- system.file("extdata", file_name, package = "kurszuweisung", mustWork = TRUE)
  return(path)
}
