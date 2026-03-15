# .............................................................................
# Persistence Logic for Course Assignment Shiny App
# .............................................................................

CONFIG_FILE <- ".shiny_config.json"

#' Load the configuration from the local JSON file
#'
#' @return A list containing the configuration or NULL if no file exists.
load_config <- function() {
  if (file.exists(CONFIG_FILE)) {
    tryCatch(
      {
        config <- jsonlite::fromJSON(CONFIG_FILE)
        # Validate paths - if files don't exist anymore, clear them
        if (!is.null(config$students_path) && !file.exists(config$students_path)) {
          config$students_path <- NULL
        }
        if (!is.null(config$courses_path) && !file.exists(config$courses_path)) {
          config$courses_path <- NULL
        }
        config
      },
      error = function(e) {
        NULL
      }
    )
  } else {
    NULL
  }
}

#' Save the configuration
#'
#' @param students_path Absolute path to students CSV
#' @param courses_path Absolute path to courses CSV
#' @param time_limit Time limit in seconds
#' @param enforce_survival Logical, enforce survival mode
save_config <- function(students_path, courses_path, time_limit, enforce_survival) {
  config <- list(
    students_path = students_path,
    courses_path = courses_path,
    time_limit = time_limit,
    enforce_survival = enforce_survival,
    last_updated = Sys.time()
  )
  tryCatch(
    {
      jsonlite::write_json(config, CONFIG_FILE, auto_unbox = TRUE, pretty = TRUE)
    },
    error = function(e) {
      warning("Could not save configuration: ", e$message)
    }
  )
}
