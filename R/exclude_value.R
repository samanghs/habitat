# Load necessary libraries
library(terra)

#' @title Exclude Specific Value from Binarized Raster or Vector Data
#' @description Removes a specified value from binarized raster or vector data.
#' @param data A binarized raster (SpatRaster) or vector (SpatVector) object.
#' @param exclude_value The value to exclude (e.g., FALSE or 0).
#' @return A filtered raster or vector object with the specified value removed.
#' @examples
#' \dontrun{
#' # Create a sample binary raster
#' # Exclude the specified value (e.g., 0)
#' filtered_data <- hb_exclude_value(raster_data, exclude_value = 0)
#' plot(filtered_data)
#'
#' # Example with a SpatVector
#' coords <- matrix(runif(20), ncol = 2)
#' polygon_data <- terra::vect(coords, type = "polygons")
#' filtered_polygon <- hb_exclude_value(polygon_data, exclude_value = 0)
#' plot(filtered_polygon)
#' }

#' @export
hb_exclude_value <- function(data, exclude_value) {
  if (inherits(data, "SpatRaster")) {
    # For raster data
    filtered_data <- data
    filtered_data[filtered_data == exclude_value] <- NA
  } else if (inherits(data, "SpatVector")) {
    # For vector data (polygon)
    filtered_data <- data[data[, "value"] != exclude_value, ]
  } else {
    stop("The input data must be a SpatRaster or SpatVector object.")
  }

  return(filtered_data)
}
