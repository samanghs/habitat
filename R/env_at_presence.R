#' @title Analyze environmental values at species presence
#' @description
#' Computes descriptive statistics and plots a histogram of an
#' environmental raster (e.g., elevation, slope) at species presence locations
#' defined by a binary/suitability raster and a threshold. This wraps the common
#' workflow of masking the environmental layer by presence, extracting values,
#' summarizing them, and visualizing their distribution.
#'
#' @param env_raster A SpatRaster (preferred) or RasterLayer of the environmental variable
#'   (e.g., elevation in meters, slope in degrees).
#' @param suit_raster A SpatRaster or RasterLayer representing species suitability or
#'   presence probability. If already binary (0/1), set \code{threshold = NULL}.
#' @param threshold Numeric or NULL. Threshold applied to \code{suit_raster} to derive
#'   presence (1) vs absence (0). If \code{NULL}, values > 0 are treated as presence.
#' @param mask_geom Optional SpatVector or SpatRaster mask/crop geometry (e.g., a border).
#'   If provided, \code{env_raster} is cropped and masked to this geometry before analysis.
#' @param plot Logical. If TRUE, a histogram is plotted immediately.
#' @param breaks Integer or vector. Breaks passed to \code{hist()} for the histogram.
#'   Defaults to "Sturges" via \code{breaks = "Sturges"} if left NULL.
#' @param col Character. Fill color for the histogram bars. Default \code{"lightgray"}.
#' @param border Character. Border color for the histogram bars. Default \code{NA}.
#' @param main Character. Plot title. Default \code{"Environmental values at presence locations"}.
#' @param xlab Character. X-axis label. Default \code{"Environmental value"}.
#' @param ylab Character. Y-axis label. Default \code{"Frequency"}.
#' @param na_rm Logical. Whether to remove NA values in summaries. Default \code{TRUE}.
#'
#' @details
#' The function aligns CRS and geometry between \code{env_raster} and \code{suit_raster}
#' and uses nearest-neighbor methods for reprojection/resampling to preserve binary
#' presence masks derived from the suitability raster. Presence cells are defined as:
#' - if \code{threshold} is numeric: \code{suit_raster >= threshold}
#' - if \code{threshold} is NULL: \code{suit_raster > 0}
#'
#' The histogram is drawn with base R \code{hist()} for speed and simplicity. Returned
#' statistics include Min, 1st Qu., Median, Mean, 3rd Qu., Max, SD, and N.
#'
#' @return A named list with:
#' \itemize{
#'   \item \code{summary}: a data.frame with Min, Q1, Median, Mean, Q3, Max, SD, N
#'   \item \code{values}: a numeric vector of extracted environmental values at presence cells
#'   \item \code{presence_mask}: the SpatRaster (0/1) presence mask used for extraction
#' }
#'
#' @examples
#' \dontrun{
#' env <- rast(system.file("ex/elev.tif", package = "terra"))
#' suit <- env / max(values(env), na.rm = TRUE)  # fake suitability 0..1
#'
#' # Analyze elevation values where suitability >= 0.5 and plot histogram
#' res <- hb_env_at_presence(
#'   env_raster = env,
#'   suit_raster = suit,
#'   threshold = 0.5,
#'   plot = TRUE,
#'   col = "lightblue",
#'   main = "Elevation at presence (>= 0.5)",
#'   xlab = "Elevation (m)"
#' )
#' res$summary
#' }
#'
#' @export
hb_env_at_presence <- function(env_raster,
                               suit_raster,
                               threshold = 0.5,
                               mask_geom = NULL,
                               plot = TRUE,
                               breaks = NULL,
                               col = "lightgray",
                               border = NA,
                               main = "Environmental values at presence locations",
                               xlab = "Environmental value",
                               ylab = "Frequency",
                               na_rm = TRUE) {
  # Dependencies
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required. Please install it.")
  }

  # Normalize inputs to SpatRaster
  if (inherits(env_raster, "Raster")) env_raster <- terra::rast(env_raster)
  if (inherits(suit_raster, "Raster")) suit_raster <- terra::rast(suit_raster)

  if (!inherits(env_raster, "SpatRaster") || !inherits(suit_raster, "SpatRaster")) {
    stop("env_raster and suit_raster must be SpatRaster or Raster* objects.")
  }

  # Optional crop/mask to geometry (SpatVector or SpatRaster)
  if (!is.null(mask_geom)) {
    # Ensure CRS compatibility
    if (inherits(mask_geom, "Spatial")) mask_geom <- terra::vect(mask_geom)
    if (inherits(mask_geom, "sf"))       mask_geom <- terra::vect(mask_geom)
    # Reproject geometry if needed
    if (!terra::same.crs(env_raster, mask_geom)) {
      mask_geom <- terra::project(mask_geom, env_raster)
    }
    env_raster  <- terra::mask(terra::crop(env_raster, mask_geom), mask_geom)
    suit_raster <- terra::mask(terra::crop(suit_raster, mask_geom), mask_geom)
  }

  # Align CRS and geometry (nearest to preserve binary behavior)
  if (!terra::same.crs(env_raster, suit_raster)) {
    suit_raster <- terra::project(suit_raster, env_raster, method = "near")
  }
  if (!terra::compareGeom(env_raster, suit_raster, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE, stopOnError = FALSE)) {
    suit_raster <- terra::resample(suit_raster, env_raster, method = "near")
  }

  # Derive presence mask
  presence_mask <- if (is.null(threshold)) {
    suit_raster > 0
  } else {
    suit_raster >= threshold
  }
  # Convert logical to 0/1 for clarity
  presence_mask <- terra::ifel(presence_mask, 1, 0)

  # Mask environmental raster by presence (maskvalue=0 removes absences)
  env_at_presence <- terra::mask(env_raster, presence_mask, maskvalue = 0)

  # Extract values
  vals <- terra::values(env_at_presence, mat = FALSE, na.rm = na_rm)

  # Compute statistics
  if (length(vals) == 0L) {
    warning("No presence cells found after thresholding/masking; returning empty results.")
    stats_df <- data.frame(
      Min = NA_real_, Q1 = NA_real_, Median = NA_real_, Mean = NA_real_,
      Q3 = NA_real_, Max = NA_real_, SD = NA_real_, N = 0L
    )
  } else {
    qs <- stats::quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = na_rm, names = FALSE)
    stats_df <- data.frame(
      Min = min(vals, na.rm = na_rm),
      Q1 = qs[1],
      Median = qs[2],
      Mean = mean(vals, na.rm = na_rm),
      Q3 = qs[3],
      Max = max(vals, na.rm = na_rm),
      SD = stats::sd(vals, na.rm = na_rm),
      N  = length(vals)
    )
  }

  # Optionally plot histogram immediately
  if (isTRUE(plot)) {
    # Choose default breaks if not supplied
    brks <- if (is.null(breaks)) "Sturges" else breaks
    graphics::hist(
      vals,
      breaks = brks,
      main = main,
      xlab = xlab,
      ylab = ylab,
      col = col,
      border = border
    )
  }

  # Console-friendly summary print
  print(stats_df, row.names = FALSE)

  # Return
  list(
    summary = stats_df,
    values = vals,
    presence_mask = presence_mask
  )
}
