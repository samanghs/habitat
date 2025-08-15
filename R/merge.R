# Load necessary libraries
library(raster)

#' @title Merge Rasters
#' @description Merges two or more rasters into a single raster file. If you encounter any issues with `RasterLayer` objects, use the `spat_to_raster()` function to convert them.
#' @param raster_list A list of `RasterLayer` objects to be merged.
#' @param output_path A character string representing the file path to save the merged raster. If `NULL`, the raster will not be saved to a file.
#' @return A `RasterLayer` object representing the merged raster.
#' @examples
#' \dontrun{
#' # Create two sample rasters or load it: "raster1" and "raster2"
#' # Merge rasters (two or more), optionally saving to a file
#' merged_raster <- hb_merge(list(raster1, raster2),
#'                            tempfile(fileext = ".tif"))
#'
#' # NOTE: 'RasterLayer' issues? Use the spat_to_raster() function
#' raster1_rl <- spat_to_raster(raster1)
#' raster2_rl <- spat_to_raster(raster2)
#'
#' # Plot the merged raster
#' plot(merged_raster)
#' }

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
