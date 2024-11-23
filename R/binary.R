#' @title Convert Raster to Binary Map
#' @description This function converts a continuous raster dataset into a binary map based on a specified threshold. Values in the raster that are greater than the threshold are converted to `TRUE`, while values less than or equal to the threshold are converted to `FALSE`.
#' @param x A RasterLayer or SpatRaster object. This represents the continuous raster dataset to be converted.
#' @param th A numeric threshold value between 0 and 1. This threshold determines the cutoff point for converting values to `TRUE` or `FALSE`.
#' @return A binary RasterLayer or SpatRaster where values greater than the specified threshold are `TRUE`, and values less than or equal to the threshold are `FALSE`.
#' @details The function is designed to take a continuous raster dataset and convert it into a binary map. This is useful for applications where a binary classification of the data is needed, such as presence/absence mapping or suitability analysis.
#' @examples
#' # Example usage with a SpatRaster object
#' library(terra)
#'
#' # Create a sample raster dataset with random values
#' r <- rast(nrows=10, ncols=10, vals=runif(100))
#'
#' # Convert the raster to a binary map using a threshold of 0.5
#' binary_map <- binary(r, th=0.5)
#'
#' # Plot the resulting binary map
#' plot(binary_map, main="Binary Map (Threshold = 0.5)")
#' @export
binary <- function(x, th) {
  if (!inherits(x, c("RasterLayer", "SpatRaster"))) {
    stop("Input must be a RasterLayer or SpatRaster object.")
  }

  if (!is.numeric(th) || th < 0 || th > 1) {
    stop("Threshold must be a number between 0 and 1.")
  }

  bin_map <- x > th
  return(bin_map)
}
