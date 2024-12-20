% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changes.R
\name{hb_plot_changes}
\alias{hb_plot_changes}
\title{Plot Habitat Changes}
\usage{
hb_plot_changes(
  changes_raster,
  title = "Changes",
  xlab = "Longitude",
  ylab = "Latitude",
  bg_color = "white"
)
}
\arguments{
\item{changes_raster}{A RasterLayer object generated by \code{hb_changes}.}

\item{title}{Optional. Title of the plot. Default is "Changes".}

\item{xlab}{Optional. Label for the x-axis. Default is "Longitude".}

\item{ylab}{Optional. Label for the y-axis. Default is "Latitude".}

\item{bg_color}{Optional. Background color of the plot. Default is "white".}
}
\description{
Plots a raster highlighting areas of habitat gain, loss, stability, and absence. This visualization helps in understanding the extent and distribution of habitat changes.
}
\details{
This function plots the habitat changes using ggplot2, with colors representing different types of changes. It provides a visual representation of how habitats have changed, which is useful for ecological assessments and decision-making.
}
\examples{
# Load sample rasters
raster1 <- raster("path/to/first_raster.tif")
raster2 <- raster("path/to/second_raster.tif")

# Convert to binary
raster1_binary <- hb_binary(raster1, th = 0.5)
raster2_binary <- hb_binary(raster2, th = 0.5)

# Calculate changes
changes_raster <- hb_changes(raster1_binary, raster2_binary)

# Plot changes
hb_plot_changes(changes_raster, title = "Habitat Changes", xlab = "X Axis", ylab = "Y Axis", bg_color = "lightgray")
}
