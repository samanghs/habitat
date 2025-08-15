#' @title Export Habitat Results to CSV
#' @description Exports habitat change metrics to a CSV file. The habitat change metrics are extracted from the provided result list and saved as a CSV file at the specified file path.
#' @param result A list containing the habitat change metrics. This list should include a component named `Compt.By.Models`, which holds the data to be exported.
#' @param file_path The file path where the CSV file will be saved. The path should include the desired file name and the `.csv` extension.
#' @return None. This function is used for its side effect of writing the data to a CSV file.
#' @details Designed to facilitate the export of habitat change metrics to a CSV file, making it easier to save analysis results in a format that can be readily shared and imported into other software for further analysis.
#' @examples
#' \dontrun{
#' # Create a sample result list with habitat change metrics
#' result <- list(
#'   Compt.By.Models = data.frame(
#'     Model = c("Model1", "Model2"),
#'     Metric1 = c(0.1, 0.2),
#'     Metric2 = c(0.3, 0.4)
#'   )
#' )
#'
#' # Export the habitat change metrics to a CSV file
#' hb_exp_csv(result, tempfile(fileext = ".csv"))
#' }
#' @export
hb_exp_csv <- function(result, file_path) {
  write.csv(result$Compt.By.Models, file_path, row.names = FALSE)
}

#' @title Export Habitat Results to TXT
#' @description Exports habitat change metrics to a TXT file. The habitat change metrics are extracted from the provided result list and saved as a TXT file at the specified file path.
#' @param result A list containing the habitat change metrics. This list should include a component named `Compt.By.Models`, which holds the data to be exported.
#' @param file_path The file path where the TXT file will be saved. The path should include the desired file name and the `.txt` extension.
#' @return None. This function is used for its side effect of writing the data to a TXT file.
#' @details Designed to facilitate the export of habitat change metrics to a TXT file, making it easier to save analysis results in a text format that can be readily shared and imported into other software for further analysis.
#' @examples
#' \dontrun{
#' # Create a sample result list with habitat change metrics
#' result <- list(
#'   Compt.By.Models = data.frame(
#'     Model = c("Model1", "Model2"),
#'     Metric1 = c(0.1, 0.2),
#'     Metric2 = c(0.3, 0.4)
#'   )
#' )
#'
#' # Export the habitat change metrics to a TXT file
#' hb_exp_txt(result, tempfile(fileext = ".txt"))
#' }
#' @export
hb_exp_txt <- function(result, file_path) {
  write.table(result$Compt.By.Models, file_path, row.names = FALSE, sep = "\t")
}

# Load necessary libraries
library(terra)

#' @title Export Raster to File
#' @description Exports a raster to a specified file format (.tif, .png, or .jpg).
#' @param raster_data A raster (SpatRaster) object to be exported.
#' @param file_path The file path where the raster will be saved. The extension should be .tif, .png, or .jpg.
#' @return None. This function is used for its side effect of writing the raster to a file.
#' @examples
#' \dontrun{
#' # Create a sample raster or load: "raster_data"
#' # Export the raster to various formats (saved to temp files)
#' hb_exp_raster(raster_data, tempfile(fileext = ".tif"))
#' hb_exp_raster(raster_data, tempfile(fileext = ".png"))
#' hb_exp_raster(raster_data, tempfile(fileext = ".jpg"))
#' }
#' @export
hb_exp_raster <- function(raster_data, file_path) {
  # Check the file extension
  ext <- tools::file_ext(file_path)

  if (!inherits(raster_data, "SpatRaster")) {
    stop("The input data must be a SpatRaster object.")
  }

  if (ext == "tif") {
    writeRaster(raster_data, file_path, format = "GTiff")
  } else if (ext == "png") {
    writeRaster(raster_data, file_path, format = "PNG")
  } else if (ext == "jpg" || ext == "jpeg") {
    writeRaster(raster_data, file_path, format = "JPEG")
  } else {
    stop("Unsupported file extension. Please use .tif, .png, or .jpg.")
  }
}
