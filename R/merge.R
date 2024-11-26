# Load necessary libraries
library(raster)

#' @title Merge Rasters
#' @description Merges two or more rasters into a single raster file. If you encounter any issues with `RasterLayer` objects, use the `spat_to_raster()` function to convert them.
#' @param raster_list A list of `RasterLayer` objects to be merged.
#' @param output_path A character string representing the file path to save the merged raster. If `NULL`, the raster will not be saved to a file.
#' @return A `RasterLayer` object representing the merged raster.
#' @examples
#' # Load sample rasters
#' raster1 <- raster("path/to/raster1.tif")
#' raster2 <- raster("path/to/raster2.tif")
#'
#' # Merge rasters (two or more), and save it in '.tif' format (optional).
#' merged_raster <- hb_merge(list(raster1, raster2), "path/to/merged_raster.tif")
#'
#' # NOTE: 'RasterLayer' issues? Use the spat_to_raster() function
#' raster1 <- spat_to_raster(rast("path/to/raster1.tif"))
#' raster2 <- spat_to_raster(rast("path/to/raster2.tif"))
#'
#' # Plot the merged raster
#' plot(merged_raster)
#'
#' @export
hb_merge <- function(raster_list, output_path = NULL) {

  if (!is.list(raster_list) || length(raster_list) < 2) {
    stop("raster_list must be a list of at least two RasterLayer objects.")
  }
  if (!all(sapply(raster_list, inherits, "RasterLayer"))) {
    stop("All elements in raster_list must be RasterLayer objects.")
  }

  raster_list <- lapply(raster_list, function(r) {
    if (is.logical(values(r))) {
      r[] <- as.numeric(r[])
    }
    return(r)
  })

  merged_raster <- do.call(mosaic, c(raster_list, fun = max))

  if (!is.null(output_path)) {
    writeRaster(merged_raster, output_path, format = "GTiff", overwrite = TRUE)
  }

  return(merged_raster)
}
