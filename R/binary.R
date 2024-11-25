#' @title Convert Raster to Binary Map
#' @description Converts a continuous raster dataset into a binary map based on a specified threshold. Values in the raster that are greater than the threshold are converted to `TRUE`, while values less than or equal to the threshold are converted to `FALSE`.
#' @param x A RasterLayer or SpatRaster object representing the continuous raster dataset to be converted.
#' @param th A numeric threshold value between 0 and 1, determining the cutoff point for converting values to `TRUE` or `FALSE`.
#' @return A binary RasterLayer or SpatRaster where values greater than the specified threshold are `TRUE`, and values less than or equal to the threshold are `FALSE`.
#' @details This function is designed to convert a continuous raster dataset into a binary map. This is particularly useful for applications requiring binary classification of data, such as presence/absence mapping or suitability analysis.
#' @examples
#' # Example usage with a SpatRaster object
#'
#'
#' # Create a sample raster dataset with random values
#' r <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Convert the raster to a binary map using a threshold of 0.5
#' binary_map <- hb_binary(r, th = 0.5)
#'
#' # Plot the resulting binary map
#' plot(binary_map, main = "Binary Map (Threshold = 0.5)")
#' @references
#' Ghasemian Sorboni, S., Hadipour, M., & Pourebrahim, S (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:.

#' @export
hb_binary <- function(x, th) {
  if (!inherits(x, c("RasterLayer", "SpatRaster"))) {
    stop("Input must be a RasterLayer or SpatRaster object.")
  }

  if (!is.numeric(th) || th < 0 || th > 1) {
    stop("Threshold must be a number between 0 and 1.")
  }

  bin_map <- x > th
  return(bin_map)
}
