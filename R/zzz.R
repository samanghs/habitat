## zzz.R

# Declare global variables for R CMD check
utils::globalVariables(c(
  "trend", "mosaic", "x", "y", "value", "Metric", "Value"
))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "\nhabitat loaded 🐾\n",
      "NOTE: Be sure you've loaded all necessary libraries for your workflow, ",
      "such as {terra} and other spatial analysis packages you rely on.\n"
    )
  )
}
