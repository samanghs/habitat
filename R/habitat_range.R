#' @title Analyze Habitat Changes
#' @description Computes metrics such as gain, loss, stable areas, and total changes between two binary raster maps. Provides a detailed analysis of habitat changes over time.
#' @param x A SpatRaster object representing the current habitat (binary). This raster should contain binary values indicating habitat presence and absence.
#' @param y A SpatRaster object representing the future habitat (binary). This raster should also contain binary values indicating habitat presence and absence.
#' @param th A numeric threshold value between 0 and 1, used to convert continuous data into a binary format before analysis.
#' @return A list containing:
#' \itemize{
#'   \item \code{Compt.By.Models}: A data frame with detailed metrics, including loss, gain, stable areas, and percentage changes.
#'   \item \code{Diff.By.Pixel}: A SpatRaster showing pixel-wise differences between the current and future habitat maps.
#' }
#' @details Designed to compare two binary raster maps representing habitat data at different time points. Calculates various metrics to summarize the changes between the two maps, which can be used to assess the impact of environmental changes or conservation efforts.
#' @examples
#' \dontrun{
#' # Create sample binary raster datasets or load it: for instance "r1" and "r2"
#' # Analyze habitat changes
#' result <- hb_range(r1, r2, th = 0.5)
#'
#' # Display the computed metrics
#' print(result$Compt.By.Models)
#'
#' # Plot the habitat changes
#' hb_plot(result$Compt.By.Models)
#' }

#' @export
hb_range <- function(x, y, th) {
  if (!inherits(x, c("SpatRaster")) || !inherits(y, c("SpatRaster"))) {
    stop("Both inputs must be SpatRaster objects.")
  }

  x_bin <- hb_binary(x, th)
  y_bin <- hb_binary(y, th)

  x_values <- terra::values(x_bin)
  y_values <- terra::values(y_bin)

  valid_indices <- !is.na(x_values) & !is.na(y_values)
  x_values <- x_values[valid_indices]
  y_values <- y_values[valid_indices]

  loss <- sum(x_values == 1 & y_values == 0)
  stable0 <- sum(x_values == 0 & y_values == 0)
  stable1 <- sum(x_values == 1 & y_values == 1)
  gain <- sum(y_values == 1 & x_values == 0)
  change <- gain + loss

  total_pixels <- length(x_values)
  pixel_area <- terra::res(x)[1] * terra::res(x)[2] / 1e6
  total_area <- total_pixels * pixel_area

  Compt.By.Models <- data.frame(
    Metric = c("Loss", "Stable0", "Stable1",
               "Gain", "PercLoss", "PercGain",
               "SpeciesRangeChange", "CurrentRangeSize",
               "FutureRangeSize.NoDisp", "FutureRangeSize.FullDisp"),
    Value = c(loss,
              stable0,
              stable1,
              gain,
              round(100 * loss / (loss + stable1), 2),
              round(100 * gain / (loss + stable1), 2),
              round(100 * (gain - loss) / (loss + stable1), 2),
              sum(x_values == 1),
              sum(y_values == 1),
              sum(y_values == 1) + gain
    )
  )

  diff_raster <- y_bin - x_bin
  diff_raster[diff_raster == -1] <- -2

  result <- list(
    Compt.By.Models = Compt.By.Models,
    Diff.By.Pixel = diff_raster
  )

  return(result)
}

#' @title Plot Habitat Changes
#' @description Plots habitat changes as a bar chart. The bar chart visualizes the percentage of loss, gain, and overall species range change.
#' @param data A data frame containing the habitat change metrics. This data frame should include the percentage metrics such as `PercLoss`, `PercGain`, and `SpeciesRangeChange`.
#' @return None. This function is used for its side effect of creating and displaying the plot.
#' @details Designed to take the habitat change metrics computed by the `hb_habitat_range` function and visualize them in a bar chart. This helps in understanding the extent of habitat changes visually.
#' @examples
#' \dontrun{
#' # Assume result is obtained from hb_habitat_range function
#' # result <- hb_habitat_range(r1, r2, th = 0.5)
#'
#' # Plot the habitat changes
#' hb_range_plot(result)
#' }
#' @export
hb_range_plot <- function(data) {
  data <- data[data$Metric %in% c("PercLoss", "PercGain", "SpeciesRangeChange"), ]

  custom_colors <- c("PercLoss" = "darkred", "PercGain" = "darkgreen", "SpeciesRangeChange" = "darkgray")

  ggplot(data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Habitat Change",
         x = "Metric",
         y = "Percentage",
         fill = "Metric") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(values = custom_colors)
}
