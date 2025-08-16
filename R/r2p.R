#' @title Convert Raster to Polygon
#' @description Converts a binary or continuous SpatRaster object to a polygon.
#' @param raster A SpatRaster object to be converted.
#' @param binary Logical. If `TRUE`, the raster will be treated as binary. Default is `FALSE`.
#' @return An sf object representing the raster converted to polygons.
#' @details The function is designed to convert a raster dataset into a polygon, which is useful for visualizing raster data as vector data and performing vector-based spatial analyses.
#' @examples
#' \dontrun{
#' # Create a sample binary raster or load it: "binary_raster"
#'
#' # Convert the binary raster to polygons
#' binary_polygons <- hb_raster_to_polygon(binary_raster, binary = TRUE)
#' plot(binary_polygons)
#'
#' # Create a sample raster or load it: "raster1"
#'
#' # Convert the raster to polygons
#' continuous_polygons <- hb_ras_to_pol(raster1)
#' plot(continuous_polygons)
#' }
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
