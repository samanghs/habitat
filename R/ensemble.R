#' @title Create Ensemble Map
#' @description
#' Combines two or more rasters into a single ensemble map
#' using either majority-based or statistical methods.
#'
#' @param rasters A list of SpatRaster or RasterLayer objects.
#' @param method Character. One of:
#'   \itemize{
#'     \item \code{"majority"} — Majority vote on binarized rasters.
#'     \item \code{"weighted_majority"} — Weighted majority vote on binarized rasters.
#'     \item \code{"mean"} — Unweighted averaging of continuous rasters.
#'     \item \code{"weighted_mean"} — Weighted averaging of continuous rasters.
#'   }
#' @param threshold Numeric or \code{NULL}. Threshold for binarizing rasters
#'   (only used with \code{"majority"} or \code{"weighted_majority"}).
#'   If \code{NULL}, defaults to 0.5.
#' @param weights Numeric vector of weights (same length as \code{rasters})
#'   used for weighted methods. Ignored for unweighted methods.
#'   If \code{NULL} and a weighted method is chosen, equal weights are assumed.
#' @param na_rm Logical. Remove NAs in calculations. Default \code{TRUE}.
#'
#' @return A SpatRaster of the ensemble habitat suitability map.
#' @details
#' **Majority methods** first binarize input rasters using the given threshold, then:
#' - \code{"majority"}: cells with > 50\% presence across rasters are 1, else 0.
#' - \code{"weighted_majority"}: uses weighted sum of presences, applies cutoff of \code{sum(weights)/2}.
#'
#' **Statistical methods** operate directly on continuous values:
#' - \code{"mean"}: cellwise unweighted mean.
#' - \code{"weighted_mean"}: cellwise weighted mean.
#'
#' All rasters are aligned in CRS, resolution, and extent before computation.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r1 <- rast(matrix(runif(100, 0, 1), 10, 10))
#' r2 <- rast(matrix(runif(100, 0, 1), 10, 10))
#' r3 <- rast(matrix(runif(100, 0, 1), 10, 10))
#'
#' # Unweighted mean
#' ens_mean <- hb_ensemble(list(r1, r2, r3), method = "mean")
#'
#' # Weighted majority with threshold 0.6
#' ens_wmaj <- hb_ensemble(list(r1, r2, r3), method = "weighted_majority",
#'                              threshold = 0.6, weights = c(0.5, 0.3, 0.2))
#' }
#'
#' @export
hb_ensemble <- function(rasters,
                        method = c("majority", "weighted_majority", "mean", "weighted_mean"),
                        threshold = NULL,
                        weights = NULL,
                        na_rm = TRUE) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.")
  }

  method <- match.arg(method)

  # Ensure list input
  if (inherits(rasters, c("SpatRaster", "RasterLayer"))) {
    rasters <- list(rasters)
  }
  if (!is.list(rasters) ||
      !all(vapply(rasters, function(r) inherits(r, c("SpatRaster", "RasterLayer")), logical(1)))) {
    stop("'rasters' must be a list of SpatRaster or RasterLayer objects.")
  }

  # Convert RasterLayer to SpatRaster
  rasters <- lapply(rasters, function(r) {
    if (inherits(r, "Raster")) terra::rast(r) else r
  })

  n <- length(rasters)
  if (n < 2) stop("At least two rasters are required.")

  # Align CRS, extent, and resolution to the first raster
  ref <- rasters[[1]]
  rasters <- lapply(rasters, function(r) {
    if (!terra::same.crs(r, ref)) {
      r <- terra::project(r, ref, method = "near")
    }
    if (!terra::compareGeom(r, ref, crs = TRUE, ext = TRUE,
                            rowcol = TRUE, res = TRUE, stopOnError = FALSE)) {
      r <- terra::resample(r, ref, method = "near")
    }
    r
  })

  # Handle weights
  if (is.null(weights) && grepl("weighted", method)) {
    weights <- rep(1 / n, n)
  }
  if (!is.null(weights) && length(weights) != n) {
    stop("Length of 'weights' must match number of rasters.")
  }

  # Apply methods
  if (method %in% c("majority", "weighted_majority")) {
    if (is.null(threshold)) threshold <- 0.5
    rasters_bin <- lapply(rasters, function(r) terra::ifel(r >= threshold, 1, 0))

    if (method == "majority") {
      sum_bin <- Reduce(`+`, rasters_bin)
      ensemble <- terra::ifel(sum_bin >= (n / 2), 1, 0)
    } else {
      wsum <- Reduce(`+`, mapply(`*`, rasters_bin, weights, SIMPLIFY = FALSE))
      ensemble <- terra::ifel(wsum >= (sum(weights) / 2), 1, 0)
    }

  } else { # mean or weighted_mean
    if (method == "mean") {
      ensemble <- Reduce(`+`, rasters) / n
    } else {
      wsum <- Reduce(`+`, mapply(`*`, rasters, weights, SIMPLIFY = FALSE))
      ensemble <- wsum / sum(weights)
    }
  }

  ensemble <- setNames(ensemble, paste0("ensemble_", method))
  ensemble
}
