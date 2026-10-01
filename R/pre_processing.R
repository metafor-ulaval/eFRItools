#' Pre-processing stage
#'
#' Using a multiband raster that will later be used for polygon segmentation in \link{generic_region_merging},
#' the function masks the unwanted pixel (rivers, lakes, roads, ...), smooth the layers, and normalizes the values to range
#' in `[0,255]`
#'
#' @param layers SpatRaster; multiband raster to segment with [generic_region_merging()] from the `terra` package.
#' @param masks list; SpatVector objects from the `terra` package used to mask the raster. `NULL` for no mask.
#'
#' @returns
#' Pre-processed SpatRaster from the `terra` package
#' @export

#' @examples
#' library(terra)
#' library(purrr)
#'
#' metrics_pre_processing <- pre_processing(rast(metrics), masks = map(masks, vect))
#'
#' metrics_pre_processing
pre_processing = function(layers, masks = NULL)
{
  onames = names(layers)

  if (!is.null(masks))
  {
    cat("Masks\n")
    for (mask in masks)
    {
      if (terra::crs(mask) != terra::crs(layers))
        mask <- terra::project(mask, layers)

      layers <- terra::mask(layers, mask, inverse = T, touches = T)
    }
  }

  cat("Deal with missing data\n")
  layers[anyNA(layers)] <- NA

  cat("Rescale in [0, 255]\n")
  layers = terra::stretch(layers, maxv = 255, minq = 0.01, maxq = 0.99)

  names(layers) = onames
  return(layers)
}
