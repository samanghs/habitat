#' @title Analyze Suitable Habitat
#' @description Analyzes the range of suitable habitat based on a given threshold and provides statistics and a histogram plot.
#' @param x1 Path to the input raster file (e.g., Elevation, NDVI).
#' @param x2 Path to the habitat suitability raster file (e.g., ensemble map).
#' @param threshold Numeric value for the presence probability threshold. Default is 0.5.
#' @return A list containing the range statistics and the histogram plot.
#' @details Calculates the range of suitable habitat areas using pre-aligned raster files.
#' @examples
#' # Example usage
#' x1 <- "path/to/DEM.tif"
#' x2 <- "path/to/habitat.tif"
#' result <- hb_analyze_habitat(x1, x2, threshold = 0.5)
#' print(result$statistics)
#' print(result$plot)
#' @export
hb_analyze_habitat <- function(x1, x2, threshold = 0.5) {
  # Load the input and habitat raster files
  input_raster <- rast(x1)
  habitat_raster <- rast(x2)

  x2_mask <- habitat_raster > threshold
  masked_raster <- mask(input_raster, x2_mask, maskvalue = 0)
  data_values <- values(masked_raster, na.rm = TRUE)

  stats <- summary(data_values)
  stats <- c(stats, SD = sd(data_values, na.rm = TRUE))
  print(stats)

  # Create a histogram plot
  histogram <- ggplot() +
    geom_histogram(aes(x = data_values), bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = "Range of Suitable Habitat", x = "Values", y = "Frequency")

  return(list(statistics = stats, plot = histogram))
}
