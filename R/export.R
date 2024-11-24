#' @title Export Habitat Results to CSV
#' @description Exports habitat change metrics to a CSV file. The habitat change metrics are extracted from the provided result list and saved as a CSV file at the specified file path.
#' @param result A list containing the habitat change metrics. This list should include a component named `Compt.By.Models`, which holds the data to be exported.
#' @param file_path The file path where the CSV file will be saved. The path should include the desired file name and the `.csv` extension.
#' @return None. This function is used for its side effect of writing the data to a CSV file.
#' @details Designed to facilitate the export of habitat change metrics to a CSV file, making it easier to save analysis results in a format that can be readily shared and imported into other software for further analysis.
#' @examples
#' # Example usage
#'
#' # Create a sample result list with habitat change metrics
#' result <- list(Compt.By.Models = data.frame(Model = c("Model1", "Model2"),
#' Metric1 = c(0.1, 0.2), Metric2 = c(0.3, 0.4)))
#'
#' # Export the habitat change metrics to a CSV file
#' hb_export_csv(result, "habitat_results.csv")
#' @export
hb_export_csv <- function(result, file_path) {
  write.csv(result$Compt.By.Models, file_path, row.names = FALSE)
}

#' @title Export Habitat Results to TXT
#' @description Exports habitat change metrics to a TXT file. The habitat change metrics are extracted from the provided result list and saved as a TXT file at the specified file path.
#' @param result A list containing the habitat change metrics. This list should include a component named `Compt.By.Models`, which holds the data to be exported.
#' @param file_path The file path where the TXT file will be saved. The path should include the desired file name and the `.txt` extension.
#' @return None. This function is used for its side effect of writing the data to a TXT file.
#' @details Designed to facilitate the export of habitat change metrics to a TXT file, making it easier to save analysis results in a text format that can be readily shared and imported into other software for further analysis.
#' @examples
#' # Example usage
#'
#' # Create a sample result list with habitat change metrics
#' result <- list(Compt.By.Models = data.frame(Model = c("Model1", "Model2"),
#' Metric1 = c(0.1, 0.2), Metric2 = c(0.3, 0.4)))
#'
#' # Export the habitat change metrics to a TXT file
#' hb_export_txt(result, "habitat_results.txt")
#' @export
hb_export_txt <- function(result, file_path) {
  write.table(result$Compt.By.Models, file_path, row.names = FALSE, sep = "\t")
}
