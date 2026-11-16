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

  # polygon id and metric value of the cells covered by a polygon
  # (only these cells are read, not the full grid)
  x$id <- seq_len(nrow(x))
  x_raster <- terra::rasterize(x, y, "id")
  cells <- terra::cells(x_raster)
  id <- as.integer(x_raster[cells][, 1])
  value <- y[cells][, 1]
  keep <- !is.na(value)
  id <- id[keep]
  value <- value[keep]

  # SST
  sst <- sum((value - mean(value))^2)

  # SSE
  ids <- sort(unique(id))
  mean_by_segment <- numeric(nrow(x))
  mean_by_segment[ids] <- rowsum(value, id)[, 1] / tabulate(id)[ids]
  sse <- sum((value - mean_by_segment[id])^2)

  # pve in r2
  pve_r2 <- 1 - (sse / sst)

  return(pve_r2)
}
