#' @title Summary Statistics for Raster Data
#' @description Provides summary statistics for a given SpatRaster object, calculating key statistical measures to summarize the distribution of values within the raster dataset.
#' @param raster A SpatRaster object representing the raster dataset for which summary statistics will be generated.
#' @return A list containing summary statistics of the raster values, including the mean, median, standard deviation, minimum, and maximum of the raster values.
#' @details Designed to take a raster dataset and compute summary statistics that provide insights into the data's distribution and variability. These statistics are useful for understanding the overall characteristics of the raster data.
#' @examples
#' # Example usage with a SpatRaster object
#'
#' # Create a sample raster dataset with random values
#' raster <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Generate the summary statistics for the raster
#' raster_summary <- hb_sstat(raster)
#'
#' # Display the summary statistics
#' print(raster_summary)
#' @references
#' Ghasemian Sorboni, S., Hadipour, M., & Pourebrahim, S (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:.

#' @export
hb_sstat <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  stats <- list(
    mean = mean(values(raster), na.rm = TRUE),
    median = median(values(raster), na.rm = TRUE),
    sd = sd(values(raster), na.rm = TRUE),
    min = min(values(raster), na.rm = TRUE),
    max = max(values(raster), na.rm = TRUE)
  )

  return(stats)
}
