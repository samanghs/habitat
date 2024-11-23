#' @title Raster Value Frequency Table
#' @description This function generates a frequency table of raster values. It calculates the frequency of each unique value in the raster, providing a summary of the distribution of values.
#' @param raster A SpatRaster object. This represents the raster dataset from which the frequency table will be generated.
#' @return A data frame containing the frequency of each raster value. The data frame includes two columns: `Value`, which represents the unique values in the raster, and `Frequency`, which indicates the number of times each value occurs.
#' @details The function is designed to take a raster dataset and create a frequency table that summarizes the distribution of values within the raster. This is useful for understanding the composition and variability of the raster data.
#' @examples
#' # Example usage with a SpatRaster object
#' library(terra)
#'
#' # Create a sample raster dataset with random values between 1 and 5
#' raster <- rast(nrows=10, ncols=10, vals=sample(1:5, 100, replace=TRUE))
#'
#' # Generate the frequency table for the raster values
#' freq_table <- frequency(raster)
#'
#' # Display the frequency table
#' print(freq_table)
#' @export
frequency <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  values <- as.vector(values(raster))
  freq_table <- as.data.frame(table(values))
  colnames(freq_table) <- c("Value", "Frequency")

  return(freq_table)
}
