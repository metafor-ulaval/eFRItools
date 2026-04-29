#' Title
#'
#' @param x spatial features; polygons from the `sf` package.
#' @param y metric; SpatRaster with one from the `terra` package.
#'
#' @returns
#' proportion of explained variance in r2
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' pve_r2 <- proportion_variance_explained(fri_polygons, rast(metrics)[["z_p95"]])
#'
#' pve_r2
proportion_variance_explained <- function(x,
                                          y){

  # convert into dataframe
  x$id <- seq_len(nrow(x))
  x_raster <- terra::rasterize(x, y, "id")
  raster_temp <- c(x_raster, y)
  rater_temp_df <- terra::as.data.frame(raster_temp, na.rm = TRUE)
  colnames(rater_temp_df) <- c("id", "value")

  # SST
  mean_global <- mean(rater_temp_df$value)
  sst <- sum((rater_temp_df$value - mean_global)^2)

  # SSE
  mean_by_segment <- tapply(rater_temp_df$value,
                            rater_temp_df$id,
                            mean)
  mean_seg_vec <- mean_by_segment[as.character(rater_temp_df$id)]
  sse <- sum((rater_temp_df$value - mean_seg_vec)^2)

  # pve in r2
  pve_r2 <- 1 - (sse / sst)

  return(pve_r2)
}
