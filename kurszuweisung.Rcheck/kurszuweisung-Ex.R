pkgname <- "kurszuweisung"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('kurszuweisung')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("example_data_path")
### * example_data_path

flush(stderr()); flush(stdout())

### Name: example_data_path
### Title: Beispieldaten-Pfade abrufen
### Aliases: example_data_path

### ** Examples

## Not run: 
##D student_file <- example_data_path("students")
##D course_file <- example_data_path("courses")
##D imported <- import_data(student_file, course_file)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
