#' Beispieldaten-Pfad abrufen
#'
#' Diese Funktion liefert den absoluten Pfad zur mitgelieferten Beispieldaten-Datei
#' (.xlsx) des Pakets. Diese Datei enthält bereits die Reiter "Schüler" und "Kurse".
#'
#' @return Einen Dateipfad (Character) zur Excel-Vorlage.
#' @export
example_data_path <- function() {
  # system.file sucht im inst/extdata Ordner des installierten Paketes
  path <- system.file("extdata", "kurszuweisung_vorlage.xlsx", package = "kurszuweisung")
  
  # Fallback fuer Entwicklungsumgebung
  if (path == "" || !file.exists(path)) {
    path <- "inst/extdata/kurszuweisung_vorlage.xlsx"
  }
  
  if (!file.exists(path)) {
    stop("Beispieldaten-Datei 'kurszuweisung_vorlage.xlsx' konnte nicht gefunden werden.")
  }
  
  return(path)
}
