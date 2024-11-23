#' @title Modify Raster Parameters
#' @description Modifies the CRS, extent, resolution, and applies crop and mask to a raster.
#' @param raster A SpatRaster object to be modified.
#' @param crs Optional. A character string specifying the new CRS (e.g., "EPSG:4326").
#' @param extent Optional. A numeric vector of four values specifying the new extent (xmin, xmax, ymin, ymax).
#' @param resolution Optional. A numeric value or a vector of two numeric values specifying the new resolution.
#' @param crop_extent Optional. A SpatExtent object or numeric vector specifying the extent to crop to.
#' @param mask Optional. A SpatRaster object to be used as a mask.
#' @return A modified SpatRaster object.
#' @examples
#' r1 <- rast(nrows=10, ncols=10, vals=runif(100))
#' r2 <- rast(nrows=11, ncols=11, vals=runif(100))
#' modified_r1 <- modify_raster(r1, crs="EPSG:4326", extent=c(0, 1000, 0, 1000))
#' modified_r2 <- modify_raster(r2, crs=modified_r1, extent=c(0, 1000, 0, 1000), crop_extent = modified_r1)
#' @export
modify_raster <- function(raster, crs=NULL, extent=NULL, resolution=NULL, crop_extent=NULL, mask=NULL) {

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
    raster <- terra::resample(raster, terra::rast(resolution=resolution))
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

