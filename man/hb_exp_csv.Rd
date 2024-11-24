% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{hb_exp_csv}
\alias{hb_exp_csv}
\title{Export Habitat Results to CSV}
\usage{
hb_exp_csv(result, file_path)
}
\arguments{
\item{result}{A list containing the habitat change metrics. This list should include a component named \code{Compt.By.Models}, which holds the data to be exported.}

\item{file_path}{The file path where the CSV file will be saved. The path should include the desired file name and the \code{.csv} extension.}
}
\value{
None. This function is used for its side effect of writing the data to a CSV file.
}
\description{
Exports habitat change metrics to a CSV file. The habitat change metrics are extracted from the provided result list and saved as a CSV file at the specified file path.
}
\details{
Designed to facilitate the export of habitat change metrics to a CSV file, making it easier to save analysis results in a format that can be readily shared and imported into other software for further analysis.
}
\examples{
# Example usage

# Create a sample result list with habitat change metrics
result <- list(Compt.By.Models = data.frame(Model = c("Model1", "Model2"),
Metric1 = c(0.1, 0.2), Metric2 = c(0.3, 0.4)))

# Export the habitat change metrics to a CSV file
hb_exp_csv(result, "habitat_results.csv")
}
