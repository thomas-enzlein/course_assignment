#' Safe NULL coalescing operator
#' @name nil_coalescing
#' @rdname nil_coalescing
#' @param a The value to check
#' @param b The value to return if a is NULL
#' @return a if not NULL, else b
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Standard-Farben fuer Visualisierungen
#' @export
pkg_colors <- list(
  boy = "#3498db",      # Blau
  girl = "#e74c3c",     # Rot
  other = "#95a5a6",    # Grau
  free = "#bdc3c7",     # Hellgrau
  full = "#c0392b",     # Dunkelrot
  interest = "#2c3e50"  # Dunkelblau (Raute/Text)
)
