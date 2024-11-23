# Habitat Range Analysis Package

The 'habitat' R Package is a straightforward and accessible tool designed to help users quickly analyze and interpret habitat changes with minimal effort. Specifically tailored for new users and those unfamiliar with complex geospatial analysis workflows, this package streamlines the process of comparing habitat distributions between current and future scenarios.

# Key Features
1. Minimal Setup: Input raster layers, set a threshold, and get instant results.
2. Fast Raster Modification: Easily modify raster parameters such as CRS, extent, resolution, and apply crop and mask in one line of code.
3. Comprehensive Metrics: Gain insights into habitat loss, gain, stability, and net change percentages and areas.
4. Seamless Visualization: Generate professional-grade plots with a single command.
5. Ready-to-Export Outputs: Save results in common formats like CSV and TXT for reporting or further analysis.

## Installation

You can install the package from GitHub:

```r
devtools::install_github("samanghs/habitat")

# Example
library(habitat)
current_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
future_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
result <- habitat_range(current_raster, future_raster, threshold=0.5)
print(result)
