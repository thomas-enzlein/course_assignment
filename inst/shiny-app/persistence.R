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
        if (!is.null(config$data_path) && !file.exists(config$data_path)) {
          config$data_path <- NULL
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
#' @param data_path Absolute path to the total Excel file
#' @param time_limit Time limit in seconds
#' @param enforce_survival Logical, enforce survival mode
#' @param use_balance_gender Logical, use gender balancing
#' @param use_balance_class Logical, use class balancing
#' @param weight_gender Numeric weight for gender balancing
#' @param weight_class Numeric weight for class balancing
save_config <- function(data_path, time_limit, enforce_survival,
                        use_balance_gender = FALSE, use_balance_class = FALSE,
                        weight_gender = 50, weight_class = 50) {
  config <- list(
    data_path = data_path,
    time_limit = time_limit,
    enforce_survival = enforce_survival,
    use_balance_gender = use_balance_gender,
    use_balance_class = use_balance_class,
    weight_gender = weight_gender,
    weight_class = weight_class,
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
