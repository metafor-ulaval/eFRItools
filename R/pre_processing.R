#' Pre-processing stage
#'
#' Using a multiband raster that will later be used for polygon segmentation in \link{generic_region_merging},
#' the function masks the unwanted pixel (rivers, lakes, roads, ...), smooth the layers, and normalizes the values to range
#' in `[0,255]`
#'
#' @param metrics SpatRaster; multiband raster to segment with [generic_region_merging()] from the `terra` package.
#' @param masks list; SpatVector objects from the `terra` package used to mask the raster. `NULL` for no mask.
#'
#' @returns
#' Pre-processed SpatRaster from the `terra` package
#' @export

#' @examples
#' library(terra)
#'
#' metrics_pre_processing <- pre_processing(rast(metrics), masks = lapply(masks, vect))
#'
#' metrics_pre_processing
pre_processing = function(metrics, masks = NULL)
{
  onames = names(metrics)

  if (!is.null(masks))
  {
    cat("Masks\n")
    for (mask in masks)
    {
      if (terra::crs(mask) != terra::crs(metrics))
        mask <- terra::project(mask, metrics)

      metrics <- terra::mask(metrics, mask, inverse = T, touches = T)
    }
  }

  cat("Deal with missing data\n")
  metrics[anyNA(metrics)] <- NA

  cat("Rescale in [0, 255]\n")
  metrics = terra::stretch(metrics, maxv = 255, minq = 0.01, maxq = 0.99)

  names(metrics) = onames
  return(metrics)
}
