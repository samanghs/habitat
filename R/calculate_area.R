#' @title Calculate Suitable and Unsuitable Areas
#' @description Calculates the suitable and unsuitable areas in hectares, square kilometers, and square meters from a binary raster.
#' @param binary_raster A SpatRaster object representing the binary raster with suitable (1/TRUE) and unsuitable (0/FALSE) areas.
#' @return None. The results are printed in the console.
#' @details This function calculates the total area of suitable and unsuitable regions in a binary raster and prints the results in hectares, square kilometers, and square meters.
#' @examples
#' # Example usage with a binary SpatRaster object
#'
#'
#' # Create a sample binary raster
#' binary_raster <- rast(nrows = 10, ncols = 10, vals = sample(c(0, 1), 100, replace = TRUE))
#'
#' # Calculate and print the suitable and unsuitable areas
#' hb_cal_area(binary_raster)
#' @references
#' Ghasemian Sorboni, S., Hadipour, M., & Pourebrahim, S (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:.

#' @export
hb_cal_area <- function(binary_raster) {
  if (!inherits(binary_raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  if (!all(terra::values(binary_raster) %in% c(0, 1, NA))) {
    stop("The raster must be binary (contain only 0 and 1 values).")
  }

  cell_area_m2 <- terra::cellSize(binary_raster, unit = "m")

  suitable_area_m2 <- sum(cell_area_m2[terra::values(binary_raster) == 1], na.rm = TRUE)
  unsuitable_area_m2 <- sum(cell_area_m2[terra::values(binary_raster) == 0], na.rm = TRUE)

  suitable_area_ha <- suitable_area_m2 / 10000
  unsuitable_area_ha <- unsuitable_area_m2 / 10000

  suitable_area_km2 <- suitable_area_m2 / 1e6
  unsuitable_area_km2 <- unsuitable_area_m2 / 1e6

  # Print the results
  cat("Suitable Area:\n")
  cat("  Square Meters (m2):", suitable_area_m2, "\n")
  cat("  Hectares (ha):", suitable_area_ha, "\n")
  cat("  Square Kilometers (km2):", suitable_area_km2, "\n\n")

  cat("Unsuitable Area:\n")
  cat("  Square Meters (m2):", unsuitable_area_m2, "\n")
  cat("  Hectares (ha):", unsuitable_area_ha, "\n")
  cat("  Square Kilometers (km2):", unsuitable_area_km2, "\n")
}
