#' @title Raster Value Frequency Table
#' @description Generates a frequency table of raster values, calculating the frequency of each unique value in the raster and providing a summary of the distribution of values.
#' @param raster A SpatRaster object representing the raster dataset from which the frequency table will be generated.
#' @return A data frame containing the frequency of each raster value. The data frame includes two columns: `Value`, representing the unique values in the raster, and `Frequency`, indicating the number of times each value occurs.
#' @details Designed to create a frequency table that summarizes the distribution of values within a raster dataset, this function is useful for understanding the composition and variability of the raster data.
#' @examples
#' \dontrun{
#' # Create a sample raster dataset with random values between 1 and 5
#' raster <- terra::rast(
#'   nrows = 10, ncols = 10,
#'   vals = sample(1:5, 100, replace = TRUE)
#' )
#'
#' # Generate the frequency table for the raster values
#' freq_table <- hb_frequency(raster)
#'
#' # Display the frequency table
#' print(freq_table)
#' }

#' @export
hb_frequency <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  values <- as.vector(values(raster))
  freq_table <- as.data.frame(table(values))
  colnames(freq_table) <- c("Value", "Frequency")

  return(freq_table)
}
