#' @title Summary Statistics for Raster Data
#' @description Provides summary statistics for a given SpatRaster object, calculating key statistical measures to summarize the distribution of values within the raster dataset.
#' @param raster A SpatRaster object representing the raster dataset for which summary statistics will be generated.
#' @return A list containing summary statistics of the raster values, including the mean, median, standard deviation, minimum, and maximum of the raster values.
#' @details Designed to take a raster dataset and compute summary statistics that provide insights into the data's distribution and variability. These statistics are useful for understanding the overall characteristics of the raster data.
#' @examples
#' \dontrun{
#' # Create a sample raster dataset or load it: "raster1"
#'
#' # Generate the summary statistics for the raster
#' raster_summary <- hb_sstat(raster1)
#'
#' # Display the summary statistics
#' print(raster_summary)
#' }
#' @export
hb_sstat <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  stats <- list(
    mean = mean(values(raster), na.rm = TRUE),
    median = median(values(raster), na.rm = TRUE),
    sd = sd(values(raster), na.rm = TRUE),
    min = min(values(raster), na.rm = TRUE),
    max = max(values(raster), na.rm = TRUE)
  )

  return(stats)
}
