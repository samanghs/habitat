## zzz.R

# Declare global variables for R CMD check
utils::globalVariables(c(
  "trend", "mosaic", "x", "y", "value", "Metric", "Value"
))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "habitat loaded. Please ensure required packages are installed."
  )
}
