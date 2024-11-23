#' @title Validate Raster Inputs
#' @description Checks whether two raster datasets have identical extent, CRS, dimensions, and resolution.
#' @param x A RasterLayer or SpatRaster object representing the first dataset.
#' @param y A RasterLayer or SpatRaster object representing the second dataset.
#' @return Returns `TRUE` if all checks pass, otherwise throws an error.
#' @examples
#' library(terra)
#' r1 <- rast(nrows=10, ncols=10, vals=runif(100))
#' r2 <- rast(nrows=10, ncols=10, vals=runif(100))
#' res_check(r1, r2) # Should return TRUE
#' @export
res_check <- function(x, y) {
  if (!inherits(x, c("RasterLayer", "SpatRaster")) ||
      !inherits(y, c("RasterLayer", "SpatRaster"))) {
    stop("Both inputs must be RasterLayer or SpatRaster objects.")
  }

  if (!identical(extent(x), extent(y))) {
    stop("The extents of x and y are not identical.")
  }

  if (!identical(crs(x), crs(y))) {
    stop("The CRS of x and y are not identical.")
  }

  if (!identical(dim(x), dim(y))) {
    stop("The dimensions of x and y are not identical.")
  }

  return(TRUE)
}
