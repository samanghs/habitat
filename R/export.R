#' @title Export Habitat Results to CSV
#' @description Exports the habitat change metrics to a CSV file.
#' @param result A list containing the habitat change metrics.
#' @param file_path The file path where the CSV file will be saved.
#' @examples
#' export_csv(result, "habitat_results.csv")
#' @export
export_csv <- function(result, file_path) {
  write.csv(result$Compt.By.Models, file_path, row.names = FALSE)
}

#' @title Export Habitat Results to TXT
#' @description Exports the habitat change metrics to a TXT file.
#' @param result A list containing the habitat change metrics.
#' @param file_path The file path where the TXT file will be saved.
#' @examples
#' export_txt(result, "habitat_results.txt")
#' @export
export_txt <- function(result, file_path) {
  write.table(result$Compt.By.Models, file_path, row.names = FALSE, sep = "\t")
}

