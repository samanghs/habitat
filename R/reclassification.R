#' @title Reclassify Raster Values
#' @description This function reclassifies raster values based on specified bins. The raster values are grouped into specified bins, and each bin is assigned a new value according to the provided values vector.
#' @param raster A SpatRaster object to be reclassified. This represents the raster dataset whose values are to be reclassified.
#' @param bins A numeric vector defining the breakpoints for reclassification. These breakpoints specify the intervals for reclassification.
#' @param values A numeric vector defining the new values for each bin. Each element in this vector corresponds to a bin defined by the `bins` vector.
#' @return A reclassified SpatRaster object. The returned raster has values that are reclassified according to the specified bins and new values.
#' @details The function is designed to take a continuous or categorical raster dataset and reclassify its values based on specified breakpoints (bins). This is useful for simplifying or categorizing raster data for further analysis, visualization, or modeling.
#' @examples
#' # Example usage with a SpatRaster object
#'
#'
#' # Create a sample raster dataset with random values
#' raster <- rast(nrows=10, ncols=10, vals=runif(100))
#'
#' # Reclassify the raster values using specified bins and new values
#' reclassified_raster <- reclass(raster, bins=c(0, 0.25, 0.5, 0.75, 1), values=c(1, 2, 3, 4))
#'
#' # Plot the resulting reclassified raster
#' plot(reclassified_raster, main="Reclassified Raster Values")
#' @export
reclass <- function(raster, bins, values) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }
  if (length(bins) != length(values) + 1) {
    stop("The length of bins should be one more than the length of values.")
  }

  reclass_matrix <- cbind(bins[-length(bins)], bins[-1], values)
  reclassified_raster <- classify(raster, reclass_matrix)

  return(reclassified_raster)
}
