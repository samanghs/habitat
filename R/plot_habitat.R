#' @title Plot Habitat Map
#' @description Plots a habitat map with enhanced visualization options using ggplot2.
#' @param raster A SpatRaster object representing the habitat map.
#' @param main Title for the plot.
#' @param lonlat Logical. If TRUE, plots the habitat map on a longitude and latitude scale.
#' @param add_north_arrow Logical. If TRUE, adds a North arrow to the plot.
#' @param background_color Character string specifying the background color of the map. Default is "white".
#' @param habitat_palette Character string specifying the palette for habitat areas.
#' @param add_legend Logical. If TRUE, includes a legend in the plot.
#' @details This function plots a habitat map with various optional parameters for enhanced visualization.
#' @examples
#' # Example usage with a SpatRaster object
#'
#' # Create a sample raster
#' raster <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Plot habitat map
#' hb_plot(raster, main = "Sample Habitat Map", lonlat = TRUE, add_north_arrow = TRUE, background_color = "lightblue", add_legend = TRUE, habitat_palette = "viridis")

#' @export
hb_plot <- function(raster, main = "Habitat Map", lonlat = TRUE, add_north_arrow = FALSE, background_color = "white", habitat_palette = "viridis", add_legend = TRUE) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  raster_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "value"

  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = habitat_palette, name = "Habitat Suitability") +
    theme_minimal() +
    labs(title = main) +
    theme(panel.background = element_rect(fill = background_color, color = NA),
          plot.background = element_rect(fill = background_color, color = NA))

  if (lonlat) {
    p <- p + coord_fixed()
  }

  if (add_north_arrow) {
    p <- p + annotation_north_arrow(location = "topright", which_north = "true",
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                    style = north_arrow_fancy_orienteering)
  }

  if (!add_legend) {
    p <- p + theme(legend.position = "none")
  }

  print(p)
}
