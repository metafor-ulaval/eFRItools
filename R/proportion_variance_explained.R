#' Proportion of the variance of a metric explained by polygons
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; single metric layer from the `terra` package.
#'
#' @returns
#' Proportion of explained variance (r2)
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

  # convert into matrix
  x$id <- seq_len(nrow(x))
  x_raster <- terra::rasterize(x, y, "id")
  rater_temp_df <- terra::values(c(x_raster, y), na.rm = TRUE)
  colnames(rater_temp_df) <- c("id", "value")

  # SST
  mean_global <- mean(rater_temp_df[,2])
  sst <- sum((rater_temp_df[,2] - mean_global)^2)

  # SSE
  mean_by_segment <- tapply(rater_temp_df[,2],
                            rater_temp_df[,1],
                            mean)
  mean_seg_vec <- mean_by_segment[as.character(rater_temp_df[,1])]
  sse <- sum((rater_temp_df[,2] - mean_seg_vec)^2)

  # pve in r2
  pve_r2 <- 1 - (sse / sst)

  return(pve_r2)
}
