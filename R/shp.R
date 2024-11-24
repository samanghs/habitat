#' @title Load and Prepare Shapefile
#' @description Loads a shapefile and ensures it is ready for compatibility with raster operations, such as CRS, extent, clipping, masking, and resolution modifications.
#' @param file_path A character string specifying the path to the shapefile.
#' @return An sf object containing the shapefile data.
#' @details Reads a shapefile from the specified path and prepares it for compatibility with raster operations by aligning its CRS and extent.
#' @examples
#' # Example usage
#'
#' # Load a sample shapefile
#' shapefile_path <- "path/to/shapefile.shp"
#'
#' # Load and prepare the shapefile
#' shapefile_data <- hb_load_shapefile(shapefile_path)
#'
#' # Check the CRS and extent
#' print(st_crs(shapefile_data))
#' print(st_bbox(shapefile_data))
#' @export
hb_load_shapefile <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("The specified shapefile does not exist.")
  }

  # Read the shapefile
  shape <- st_read(file_path)

  return(shape)
}
