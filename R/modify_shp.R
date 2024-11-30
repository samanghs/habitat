#' @title Modify Shapefile Parameters
#' @description Modifies the CRS, applies cropping, masking, and extent adjustments to a shapefile based on specified parameters or derived from another shapefile.
#' @param shapefile An sf object to be modified. This represents the shapefile dataset that will undergo modifications.
#' @param crs Optional. A character string specifying the new Coordinate Reference System (CRS) (e.g., "EPSG:4326") or an sf object to derive the CRS from. If provided, the shapefile will be reprojected to this CRS.
#' @param extent Optional. An sf object specifying the new extent. If provided, the shapefile will be modified to match this extent.
#' @param crop Optional. An sf object specifying the extent to crop to. If provided, the shapefile will be cropped to this extent.
#' @param mask Optional. An sf object to be used as a mask. If provided, the shapefile will be masked by this shapefile.
#' @return A modified sf object with the specified modifications applied.
#' @details Designed to provide a comprehensive set of modifications to a shapefile dataset. This includes changing the CRS, adjusting the extent, cropping, and masking using other shapefiles.
#' @examples
#' # Example usage with sf objects
#'
#' # Load a sample shapefile
#' shapefile_path <- "path/to/shapefile.shp"
#' shapefile <- st_read(shapefile_path)
#'
#' # Load another sample shapefile to use for operations
#' shapefile2_path <- "path/to/shapefile2.shp"
#' shapefile2 <- st_read(shapefile2_path)
#'
#' # Modify the CRS using another shapefile's CRS
#' modified_shapefile <- hb_modify_shp(shapefile, crs = shapefile2)
#' plot(modified_shapefile, main = "Modified Shapefile (CRS)")
#'
#' # Modify the extent using another shapefile's extent
#' modified_shapefile <- hb_modify_shp(shapefile, extent = shapefile2)
#' plot(modified_shapefile, main = "Modified Shapefile (Extent)")
#'
#' # Crop the shapefile using another shapefile
#' modified_shapefile <- hb_modify_shp(shapefile, crop = shapefile2)
#' plot(modified_shapefile, main = "Cropped Shapefile")
#'
#' # Apply a mask to the shapefile using another shapefile
#' modified_shapefile <- hb_modify_shp(shapefile, mask = shapefile2)
#' plot(modified_shapefile, main = "Masked Shapefile")

#' @export
hb_modify_shp <- function(shapefile, crs = NULL, extent = NULL, crop = NULL, mask = NULL) {
  if (!inherits(shapefile, "sf")) {
    stop("The input must be an sf object.")
  }

  if (!is.null(crs)) {
    if (inherits(crs, "sf")) {
      crs <- st_crs(crs)
    }
    shapefile <- st_transform(shapefile, crs)
  }

  if (!is.null(extent)) {
    if (inherits(extent, "sf")) {
      bbox <- st_bbox(extent)
      shapefile <- st_crop(shapefile, bbox)
    } else {
      stop("Extent must be an sf object.")
    }
  }

  if (!is.null(crop)) {
    if (inherits(crop, "sf")) {
      shapefile <- st_intersection(shapefile, crop)
    } else {
      stop("Crop extent must be an sf object.")
    }
  }

  if (!is.null(mask)) {
    if (inherits(mask, "sf")) {
      shapefile <- st_intersection(shapefile, mask)
    } else {
      stop("Mask must be an sf object.")
    }
  }

  return(shapefile)
}
