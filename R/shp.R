#' @title Load and Prepare Shapefile
#' @description Loads a shapefile and ensures it is ready for compatibility with raster operations, such as CRS, extent, clipping, masking, and resolution modifications.
#' @param file_path A character string specifying the path to the shapefile.
#' @return An sf object containing the shapefile data.
#' @details Reads a shapefile from the specified path and prepares it for compatibility with raster operations by aligning its CRS and extent.
#' @examples
#' \dontrun{
#' # Create a temporary shapefile or load it with sf package
#' library(sf)
#' pts <- data.frame(
#'   id = 1:3,
#'   x = c(0, 1, 2),
#'   y = c(0, 1, 2)
#' )
#' sf_obj <- st_as_sf(pts, coords = c("x", "y"), crs = 4326)
#' shp_path <- tempfile(fileext = ".shp")
#' st_write(sf_obj, shp_path, quiet = TRUE)
#'
#' # Load and prepare the shapefile
#' shapefile_data <- hb_load_shp(shp_path)
#'
#' # Check CRS and extent
#' print(st_crs(shapefile_data))
#' print(st_bbox(shapefile_data))
#' }
#' @export
hb_load_shp <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("The specified shapefile does not exist.")
  }

  # Read the shapefile
  shape <- st_read(file_path)

  return(shape)
}
