#' @title Load Essential Libraries for habitat
#' @description This file ensures that all necessary libraries are installed and loaded when the habitat package is loaded.

.onAttach <- function(libname, pkgname) {

  required_packages <- c(
    "terra", "ggplot2"
  )

  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages)) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }

  for(pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
 }

