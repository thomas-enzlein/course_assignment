# Setup R Package Environment
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required setup packages if missing
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")

dir_path <- "C:/Users/Thomas/Dropbox/R/kurszuweisung"

# Create package
usethis::create_package(dir_path, open = FALSE, check_name = FALSE)

# Set active project
setwd(dir_path)
usethis::proj_set(dir_path)

# Add testthat and roxygen2
usethis::use_testthat()
usethis::use_roxygen_md()

# Initialize renv
renv::init(bare = TRUE, restart = FALSE)

cat("\nPackage setup complete!\n")
