library(ggplot2)
library(terra)
library(tidyr)

# Custom trend analysis function with NA handling
trend_analysis <- function(values) {
  values <- na.omit(values)  # Remove NA values
  if (length(values) > 1) {  # Ensure there are enough values to fit a model
    time_points <- 1:length(values)
    trend <- lm(values ~ time_points)$coefficients[2]  # Slope of the trend line
  } else {
    trend <- NA  # Not enough data to fit a model
  }
  return(trend)
}

#' @title Create Multidimensional Raster Layer
#' @description Converts a series of raster files or loaded raster objects into a multidimensional dataset for time analysis.
#'              This is the first step in a sequence of functions to analyze habitat trends.
#' @param raster_files A character vector of file paths to raster files or a list of `SpatRaster` objects.
#' @param time_points A numeric vector of time points corresponding to the raster files (e.g., 2022, 2023).
#' @return A `SpatRaster` object representing the multidimensional dataset.
#' @examples
#' \dontrun{
#' # Create sample rasters or load it: "r1" and "r2"
#' raster_files <- list(r1, r2)
#' time_points <- c(2022, 2023)
#'
#' # Create multidimensional raster layer
#' multidimensional_raster <- hb_multiD_raster(raster_files, time_points)
#'
#' # Aggregate to compute the mean
#' aggregated_raster <- hb_agg_md_raster(multidimensional_raster, fun = mean)
#'
#' # Analyze changes
#' trends <- hb_analyze_changes(multidimensional_raster, fun = trend_analysis)
#'
#' # Plot the time series data
#' hb_plot_timesrs(trends, output_path = tempfile(fileext = ".tif"))
#' }
#' @export
hb_multiD_raster <- function(raster_files, time_points) {
  rasters <- lapply(raster_files, function(x) {
    if (is.character(x)) {
      return(rast(x))
    } else if (inherits(x, "SpatRaster")) {
      return(x)
    } else {
      stop("Each element in raster_files must be a file path (character) or a SpatRaster object.")
    }
  })
  multidimensional_raster <- rast(rasters)

  time(multidimensional_raster) <- as.numeric(time_points)

  return(multidimensional_raster)
}

#' @title Aggregate Multidimensional Raster
#' @description Summarizes the multidimensional dataset across a specific dimension (e.g., calculating the average habitat suitability over time).
#'              This is the second step in a sequence of functions to analyze habitat trends.
#' @param multidimensional_raster A `SpatRaster` object representing the multidimensional dataset.
#' @param fun A function to apply for aggregation (e.g., mean, sum).
#' @return A `SpatRaster` object representing the aggregated raster.
#' @examples
#' \dontrun{
#' # Same synthetic setup as in multidimensional
#' raster_files <- list(r1, r2)
#' time_points <- c(2022, 2023)
#'
#' multidimensional_raster <- hb_multiD_raster(raster_files, time_points)
#' aggregated_raster <- hb_agg_md_raster(multidimensional_raster, fun = mean)
#' trends <- hb_analyze_changes(multidimensional_raster, fun = trend_analysis)
#'
#' hb_plot_timesrs(trends, output_path = tempfile(fileext = ".tif"))
#' }
#' @export
hb_agg_md_raster <- function(multidimensional_raster, fun = mean) {
  aggregated_raster <- tapp(multidimensional_raster, index = time(multidimensional_raster), fun = fun)

  return(aggregated_raster)
}

#' @title Analyze Changes Using Statistical Summaries
#' @description Applies statistical functions to identify trends, anomalies, or patterns in habitat changes.
#'              This is the third step in a sequence of functions to analyze habitat trends.
#' @param multidimensional_raster A `SpatRaster` object representing the multidimensional dataset.
#' @param fun A function to compute the statistical summary (e.g., trend, anomaly detection).
#' @return A data frame containing the statistical summaries.
#' @examples
#' \dontrun{
#' raster_files <- list(r1, r2)
#' time_points <- c(2022, 2023)
#'
#' multidimensional_raster <- hb_multiD_raster(raster_files, time_points)
#' aggregated_raster <- hb_agg_md_raster(multidimensional_raster, fun = mean)
#'
#' # NOTE: Depending on your dataset, calculations may take time
#' trends <- hb_analyze_changes(multidimensional_raster, fun = trend_analysis)
#'
#' hb_plot_timesrs(trends, output_path = tempfile(fileext = ".tif"))
#' }
#' @export
hb_analyze_changes <- function(multidimensional_raster, fun = trend) {
  summary_stats <- app(multidimensional_raster, fun)

  return(summary_stats)
}

#' @title Plot Time Series Data
#' @description Visualizes the trends in habitat suitability. This is the final step in a sequence of functions to analyze and visualize habitat trends.
#' @param trends A `SpatRaster` object representing the trend data.
#' @param lon_min Minimum longitude for plotting the map.
#' @param lon_max Maximum longitude for plotting the map.
#' @param lat_min Minimum latitude for plotting the map.
#' @param lat_max Maximum latitude for plotting the map.
#' @param low_color Color for the low end of the gradient.
#' @param mid_color Color for the midpoint of the gradient.
#' @param high_color Color for the high end of the gradient.
#' @param x_label Custom x-axis label.
#' @param y_label Custom y-axis label.
#' @param plot_title Custom plot title.
#' @param bg_color Background color for the plot.
#' @param output_path A character string representing the file path to save the plot. If `NULL`, the plot will not be saved to a file.
#' @return A ggplot object representing the time series plot.
#' @examples
#' \dontrun{
#' # Create dummy trends data for plotting
#' trends <- list(data = matrix(runif(100), ncol = 10))
#'
#' hb_plot_timesrs(trends,
#'   lon_min = -10, lon_max = 10, lat_min = 35, lat_max = 45,
#'   low_color = "blue", mid_color = "white", high_color = "red",
#'   x_label = "Longitude", y_label = "Latitude",
#'   plot_title = "My Custom Title",
#'   bg_color = "black",
#'   output_path = tempfile(fileext = ".tif")
#' )
#' }
#' @export
hb_plot_timesrs <- function(trends, lon_min = NULL, lon_max = NULL, lat_min = NULL, lat_max = NULL,
                            low_color = "darkred", mid_color = "gray90", high_color = "darkblue",
                            x_label = "Longitude", y_label = "Latitude", plot_title = "Habitat Suitability Trends",
                            bg_color = "white", output_path = NULL) {
  trends_df <- as.data.frame(trends, xy = TRUE) %>%
    pivot_longer(cols = -c(x, y), names_to = "variable", values_to = "value") %>%
    mutate(value = ifelse(is.na(value), 0, value))  # Handle NA values

  p <- ggplot(trends_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(low = low_color, mid = mid_color, high = high_color, midpoint = 0, na.value = "white") +
    labs(x = x_label, y = y_label, title = plot_title, fill = "Trend") +
    theme_minimal(base_family = "sans") +
    theme(plot.background = element_rect(fill = bg_color),
          panel.background = element_rect(fill = bg_color),
          panel.grid = element_blank()) +
    coord_fixed(ratio = 1)

  if (!is.null(lon_min) && !is.null(lon_max) && !is.null(lat_min) && !is.null(lat_max)) {
    p <- p + coord_fixed(ratio = 1, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))
  }

  if (!is.null(output_path)) {
    ggsave(output_path, plot = p, width = 10, height = 8, dpi = 600)
  }

  return(p)
}
