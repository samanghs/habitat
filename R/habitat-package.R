#' @keywords internal
"_PACKAGE"

## usethis namespace: start
# magrittr
#' @importFrom magrittr %>%

# terra
#' @importFrom terra rast mask values tapp time app crs project resample lapp classify compareGeom ext time<- as.polygons

# sf
#' @importFrom sf st_read st_crs st_transform st_bbox st_crop st_intersection st_as_sf

# raster
#' @importFrom raster extent mosaic writeRaster

# stats
#' @importFrom stats lm median na.omit sd setNames time

# utils
#' @importFrom utils write.csv write.table globalVariables

# methods
#' @importFrom methods as

# graphics
#' @importFrom graphics par

# ggplot2
#' @importFrom ggplot2 ggplot geom_histogram aes theme_minimal labs element_blank
#' @importFrom ggplot2 geom_raster scale_fill_viridis_c theme element_rect coord_fixed
#' @importFrom ggplot2 scale_fill_gradient2 ggsave geom_bar scale_y_continuous scale_fill_manual

# ggspatial
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering

# dplyr
#' @importFrom dplyr mutate

# tidyr
#' @importFrom tidyr pivot_longer
#' @importFrom grid unit
#' @importFrom scales percent_format
#' @importFrom sdm getEvaluation

## usethis namespace: end
NULL
