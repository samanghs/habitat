#' @title Convert Raster to Polygon
#' @description Converts a binary or continuous SpatRaster object to a polygon.
#' @param raster A SpatRaster object to be converted.
#' @param binary Logical. If `TRUE`, the raster will be treated as binary. Default is `FALSE`.
#' @return An sf object representing the raster converted to polygons.
#' @details The function is designed to convert a raster dataset into a polygon, which is useful for visualizing raster data as vector data and performing vector-based spatial analyses.
#' @examples
#' # Example usage with SpatRaster objects
#'
#' # Create a sample binary raster
#' binary_raster <- rast(nrows = 10, ncols = 10, vals = sample(c(0, 1), 100, replace = TRUE))
#'
#' # Convert the binary raster to polygons
#' binary_polygons <- hb_raster_to_polygon(binary_raster, binary = TRUE)
#'
#' # Plot the resulting polygons
#' plot(binary_polygons)
#'
#' # Create a sample continuous raster
#' continuous_raster <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Convert the continuous raster to polygons
#' continuous_polygons <- hb_ras_to_pol(continuous_raster)
#'
#' # Plot the resulting polygons
#' plot(continuous_polygons)
#' @references
#' Ghasemian Sorboni, S., Hadipour, M., & Pourebrahim, S (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:.

#' @export
hb_ras_to_pol <- function(raster, binary = FALSE) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  if (binary) {
    raster <- rast(raster)
    raster[raster != 1] <- NA
    raster <- terra::clamp(raster, lower = 1, upper = 1, values = TRUE)
  }

  polygons <- as.polygons(raster, dissolve = TRUE)
  polygons_sf <- st_as_sf(polygons)

  return(polygons_sf)
}
