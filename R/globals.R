utils::globalVariables(c("j", "x", "y", "active", "MIPModel", "add_variable", "sum_expr", "set_objective", "solve_model", "with_ROI", "get_solution"))

# Dummy calls to ensure renv detects these as dependencies
if (FALSE) {
  library(DT)
  library(bslib)
  library(jsonlite)
  library(plotly)
  library(shinyFiles)
  library(shinyjs)
  library(rmarkdown)
  library(pandoc)
  library(knitr)
  library(testthat)
  library(rcmdcheck)
  library(waldo)
}
NULL
