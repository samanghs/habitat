#' @title Summary Statistics for Raster Data
#' @description This function provides summary statistics for a given SpatRaster object. It calculates key statistical measures to summarize the distribution of values within the raster dataset.
#' @param raster A SpatRaster object. This represents the raster dataset for which summary statistics will be generated.
#' @return A list containing summary statistics of the raster values. The list includes the mean, median, standard deviation, minimum, and maximum of the raster values.
#' @details The function is designed to take a raster dataset and compute summary statistics that provide insights into the data's distribution and variability. These statistics are useful for understanding the overall characteristics of the raster data.
#' @examples
#' # Example usage with a SpatRaster object
#' library(terra)
#'
#' # Create a sample raster dataset with random values
#' raster <- rast(nrows=10, ncols=10, vals=runif(100))
#'
#' # Generate the summary statistics for the raster
#' raster_summary <- sstat(raster)
#'
#' # Display the summary statistics
#' print(raster_summary)
#' @export
sstat <- function(raster) {
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
