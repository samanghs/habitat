% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclassification.R
\name{hb_reclass}
\alias{hb_reclass}
\title{Reclassify Raster Values}
\usage{
hb_reclass(raster, bins, values)
}
\arguments{
\item{raster}{A SpatRaster object to be reclassified. Represents the raster dataset whose values are to be reclassified.}

\item{bins}{A numeric vector defining the breakpoints for reclassification. These breakpoints specify the intervals for reclassification.}

\item{values}{A numeric vector defining the new values for each bin. Each element in this vector corresponds to a bin defined by the \code{bins} vector.}
}
\value{
A reclassified SpatRaster object with values reclassified according to the specified bins and new values.
}
\description{
Reclassifies raster values based on specified bins. The raster values are grouped into specified bins, and each bin is assigned a new value according to the provided values vector.
}
\details{
Designed to take a continuous or categorical raster dataset and reclassify its values based on specified breakpoints (bins). This is useful for simplifying or categorizing raster data for further analysis, visualization, or modeling.
}
\examples{
# Example usage with a SpatRaster object

# Create a sample raster dataset with random values
raster <- rast(nrows = 10, ncols = 10, vals = runif(100))

# Reclassify the raster values using specified bins and new values
reclassified_raster <- hb_reclass(raster, bins = c(0, 0.25, 0.5, 0.75, 1), values = c(1, 2, 3, 4))

# Plot the resulting reclassified raster
plot(reclassified_raster, main = "Reclassified Raster Values")
}
