# Habitat Range Analysis Package

This package provides tools for analyzing habitat changes between current and future projections.

## Installation

You can install the package from GitHub:

```r
devtools::install_github("samanghs/habitat")
library(habitat)
current_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
future_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
result <- habitat_range(current_raster, future_raster, threshold=0.5)
print(result)
