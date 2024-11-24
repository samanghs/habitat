#' @title Stack Raster Layers
#' @description Stacks multiple SpatRaster objects into a single SpatRaster object.
#' @param rasters A list of SpatRaster objects to be stacked.
#' @return A SpatRaster object that combines all input raster layers.
#' @details This function takes a list of SpatRaster objects and stacks them into a single SpatRaster object. It is useful for combining multiple raster layers for further analysis.
#' @examples
#' # Example usage with SpatRaster objects
#'
#' # Create sample raster datasets
#' raster1 <- rast(nrows = 10, ncols = 10, vals = runif(100))
#' raster2 <- rast(nrows = 10, ncols = 10, vals = runif(100))
#'
#' # Stack the rasters
#' stacked_rasters <- hb_stack_rasters(list(raster1, raster2))
#' plot(stacked_rasters)
#' @export
hb_stack_rasters <- function(rasters) {
  if (!all(sapply(rasters, inherits, "SpatRaster"))) {
    stop("All items in the list must be SpatRaster objects.")
  }

  # Stack the raster layers
  stacked_raster <- terra::rast(rasters)
  return(stacked_raster)
}
#' @title Stack Data Frames
#' @description Stacks multiple data frames into a single data frame.
#' @param dfs A list of data frames to be stacked.
#' @return A single data frame that combines all input data frames by row binding.
#' @details This function takes a list of data frames and stacks them into a single data frame by row binding. It is useful for combining multiple data frames for further analysis.
#' @examples
#' # Example usage with data frames
#'
#' # Create sample data frames
#' df1 <- data.frame(a = 1:5, b = letters[1:5])
#' df2 <- data.frame(a = 6:10, b = letters[6:10])
#'
#' # Stack the data frames
#' stacked_df <- hb_stack_dfs(list(df1, df2))
#' print(stacked_df)
#' @export
hb_stack_dfs <- function(dfs) {
  if (!all(sapply(dfs, inherits, "data.frame"))) {
    stop("All items in the list must be data frames.")
  }

  # Stack the data frames by row binding
  stacked_df <- do.call(rbind, dfs)
  return(stacked_df)
}
