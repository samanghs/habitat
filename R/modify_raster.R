#' @title Modify Raster Parameters
#' @description Modifies the CRS, extent, resolution, and applies crop and mask operations to a raster, providing a flexible method for adjusting the spatial parameters of a raster dataset.
#' @param raster A SpatRaster object to be modified. Represents the raster dataset undergoing modifications.
#' @param crs Optional. A character string specifying the new Coordinate Reference System (CRS) (e.g., "EPSG:4326"). If provided, the raster will be reprojected to this CRS.
#' @param extent Optional. A numeric vector of four values specifying the new extent (xmin, xmax, ymin, ymax). If provided, the extent of the raster will be modified accordingly.
#' @param resolution Optional. A numeric value or a vector of two numeric values specifying the new resolution. If provided, the raster will be resampled to this resolution.
#' @param crop_extent Optional. A SpatExtent object or numeric vector specifying the extent to crop to. If provided, the raster will be cropped to this extent.
#' @param mask Optional. A SpatRaster object to be used as a mask. If provided, the raster will be masked by this raster.
#' @return A modified SpatRaster object with the specified modifications applied.
#' @details Designed to provide a comprehensive set of modifications to a raster dataset, including changing the CRS, adjusting the extent, resampling the resolution, cropping to a specified extent, and applying a mask. These modifications are useful for preparing raster data for analysis, visualization, or integration with other spatial datasets.
#' @examples
#' # Example usage with SpatRaster objects
#'
#' # Create sample raster datasets
#' r1 <- rast(nrows = 10, ncols = 10, vals = runif(100))
#' r2 <- rast(nrows = 11, ncols = 11, vals = runif(100))
#'
#' # Modify the CRS and extent of the first raster
#' modified_r1 <- hb_modify_raster(r1, crs = "EPSG:4326", extent = c(0, 1000, 0, 1000))
#' plot(modified_r1, main = "Modified Raster (CRS and Extent)")
#'
#' # Modify the CRS, extent, and crop the second raster
#' modified_r2 <- hb_modify_raster(r2, crs = "EPSG:4326", extent = c(0, 1000, 0, 1000), crop_extent = modified_r1)
#' plot(modified_r2, main = "Modified Raster (CRS, Extent, and Crop)")
#'
#' # Apply a mask to the first raster
#' mask <- rast(nrows = 10, ncols = 10, vals = sample(c(0, 1), 100, replace = TRUE))
#' masked_r1 <- hb_modify_raster(r1, mask = mask)
#' plot(masked_r1, main = "Masked Raster")
#' @export
hb_modify_raster <- function(raster, crs = NULL, extent = NULL, resolution = NULL, crop_extent = NULL, mask = NULL) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  if (!is.null(crs)) {
    raster <- terra::project(raster, crs)
  }

  if (!is.null(extent)) {
    if (length(extent) != 4) {
      stop("Extent must be a vector of four values: c(xmin, xmax, ymin, ymax).")
    }
    raster <- terra::extend(raster, terra::ext(extent))
  }

  if (!is.null(resolution)) {
    if (length(resolution) == 1) {
      resolution <- c(resolution, resolution)
    }
    raster <- terra::resample(raster, terra::rast(resolution = resolution))
  }

  if (!is.null(crop_extent)) {
    raster <- terra::crop(raster, crop_extent)
  }

  if (!is.null(mask)) {
    if (!inherits(mask, "SpatRaster")) {
      stop("Mask must be a SpatRaster object.")
    }
    raster <- terra::mask(raster, mask)
  }

  return(raster)
}
