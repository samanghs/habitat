#' @title Validate Raster Inputs
#' @description Checks whether two raster datasets have identical extent, CRS (Coordinate Reference System), dimensions, and resolution, ensuring compatibility for further analysis.
#' @param x A RasterLayer or SpatRaster object representing the first dataset. This is the initial raster that will be compared.
#' @param y A RasterLayer or SpatRaster object representing the second dataset. This is the raster that will be compared against the first raster.
#' @return Returns `TRUE` if all checks pass, otherwise throws an error indicating which check failed.
#' @details Designed to validate the compatibility of two raster datasets by checking their extent, CRS, dimensions, and resolution. This is crucial for ensuring that subsequent spatial analyses can be performed accurately without encountering alignment issues.
#' @examples
#' # Example usage with SpatRaster objects
#'
#' # Create sample raster datasets
#' r1 <- rast(nrows = 10, ncols = 10, vals = runif(100))
#' r2 <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Validate that the raster datasets have identical properties
#' hb_res_check(r1, r2) # Should return TRUE if all checks pass
#' @export
hb_res_check <- function(x, y) {
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
