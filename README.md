# habitat: An R Package for Analyzing and Comparing Habitat Changes

The 'habitat' R Package is a straightforward and user-friendly tool designed to help users quickly analyze and interpret habitat changes with minimal effort. Tailored for new users, those unfamiliar with complex geospatial analysis workflows, and researchers, this package streamlines the process of comparing habitat changes and distributions between current and future scenarios.

# Key Features
1. Minimal Setup: Input raster layers, set a threshold, and get instant results.
2. Fast Raster Modification: Easily modify raster parameters such as CRS, extent, resolution, and apply crop and mask in one line of code.
3. Comprehensive Metrics: Gain insights into habitat loss, gain, stability, and net change percentages and areas.
4. Seamless Visualization: Generate professional-grade plots with a single command.
5. Integration with sdm Package: Obtain raster results from the sdm package along with various threshold metrics for calculating, analyzing, and visualizing habitat changes.
6. Ready-to-Export Outputs: Save results in common formats like CSV and TXT for reporting or further analysis.
# Authors
- [Saman Ghasemian Sorboni](https://scholar.google.com/citations?user=FdRK6TAAAAAJ&hl=en)
- [Mehrdad Hadipour](https://scholar.google.com/citations?user=eFFA8c8AAAAJ&hl=en)

# Contributing
We welcome contributions from the community! If you have any suggestions or improvements, please infrom us! <samangh.edu@gmail.com>
# Citation
If you use habitat in your research, please cite it as follows:
Ghasemian Sorboni, S., & Hadipour, M. (2024). habitat: An R Package for Analyzing and Comparing Habitat Changes. dataset. doi:10.13140/RG.2.2.19739.99360.


## Installation

install the package from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

devtools::install_github("samanghs/habitat")


# Example
library(habitat)
current_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
future_raster <- rast(nrows=10, ncols=10, vals=sample(c(0, 1), 100, replace=TRUE))
result <- habitat_range(current_raster, future_raster, threshold=0.5)
print(result)
