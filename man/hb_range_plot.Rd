% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/habitat_range.R
\name{hb_range_plot}
\alias{hb_range_plot}
\title{Plot Habitat Changes}
\usage{
hb_range_plot(data)
}
\arguments{
\item{data}{A data frame containing the habitat change metrics. This data frame should include the percentage metrics such as \code{PercLoss}, \code{PercGain}, and \code{SpeciesRangeChange}.}
}
\value{
None. This function is used for its side effect of creating and displaying the plot.
}
\description{
Plots habitat changes as a bar chart. The bar chart visualizes the percentage of loss, gain, and overall species range change.
}
\details{
Designed to take the habitat change metrics computed by the \code{hb_habitat_range} function and visualize them in a bar chart. This helps in understanding the extent of habitat changes visually.
}
\examples{
# Example usage with the results from hb_habitat_range function

# Assume result is obtained from hb_habitat_range function
# result <- hb_habitat_range(r1, r2, th = 0.5)

# Plot the habitat changes
hb_range_plot(result$Compt.By.Models)
}
\references{
Ghasemian Sorboni, S., Hadipour, M., & Pourebrahim, S (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:.
}
