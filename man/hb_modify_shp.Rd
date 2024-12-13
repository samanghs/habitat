% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_shp.R
\name{hb_modify_shp}
\alias{hb_modify_shp}
\title{Modify Shapefile Parameters}
\usage{
hb_modify_shp(shapefile, crs = NULL, extent = NULL, crop = NULL, mask = NULL)
}
\arguments{
\item{shapefile}{An sf object to be modified. This represents the shapefile dataset that will undergo modifications.}

\item{crs}{Optional. A character string specifying the new Coordinate Reference System (CRS) (e.g., "EPSG:4326") or an sf object to derive the CRS from. If provided, the shapefile will be reprojected to this CRS.}

\item{extent}{Optional. An sf object specifying the new extent. If provided, the shapefile will be modified to match this extent.}

\item{crop}{Optional. An sf object specifying the extent to crop to. If provided, the shapefile will be cropped to this extent.}

\item{mask}{Optional. An sf object to be used as a mask. If provided, the shapefile will be masked by this shapefile.}
}
\value{
A modified sf object with the specified modifications applied.
}
\description{
Modifies the CRS, applies cropping, masking, and extent adjustments to a shapefile based on specified parameters or derived from another shapefile.
}
\details{
Designed to provide a comprehensive set of modifications to a shapefile dataset. This includes changing the CRS, adjusting the extent, cropping, and masking using other shapefiles.
}
\examples{
# Example usage with sf objects

# Load a sample shapefile
shapefile_path <- "path/to/shapefile.shp"
shapefile <- st_read(shapefile_path)

# Load another sample shapefile to use for operations
shapefile2_path <- "path/to/shapefile2.shp"
shapefile2 <- st_read(shapefile2_path)

# Modify the CRS using another shapefile's CRS
modified_shapefile <- hb_modify_shp(shapefile, crs = shapefile2)
plot(modified_shapefile, main = "Modified Shapefile (CRS)")

# Modify the extent using another shapefile's extent
modified_shapefile <- hb_modify_shp(shapefile, extent = shapefile2)
plot(modified_shapefile, main = "Modified Shapefile (Extent)")

# Crop the shapefile using another shapefile
modified_shapefile <- hb_modify_shp(shapefile, crop = shapefile2)
plot(modified_shapefile, main = "Cropped Shapefile")

# Apply a mask to the shapefile using another shapefile
modified_shapefile <- hb_modify_shp(shapefile, mask = shapefile2)
plot(modified_shapefile, main = "Masked Shapefile")
}
