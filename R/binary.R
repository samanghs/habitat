#' @title Convert Raster to Binary Map
#' @description Converts a continuous raster dataset into a binary map based on a specified threshold.
#' @param x A RasterLayer or SpatRaster object.
#' @param th A numeric threshold value between 0 and 1.
#' @return A binary RasterLayer or SpatRaster where values greater than `th` are `TRUE`, and others are `FALSE`.
#' @examples
#' library(terra)
#' r <- rast(nrows=10, ncols=10, vals=runif(100))
#' binary_map <- binary(r, th=0.5)
#' plot(binary_map)
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
