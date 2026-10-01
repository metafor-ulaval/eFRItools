#' Extract one or multiple metrics from polygons
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; metrics with one or more layers from the `terra` package.
#' @param fun character; summary function of `exactextractr::exact_extract()` (e.g. "mean", "median").
#'
#' @returns
#' Polygons with extracted metrics as individual columns
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_metrics <- mutate_metrics(fri_polygons,
#'                                        rast(metrics))
#'
#' fri_polygons_metrics
#'
#' fri_polygons_metrics <- mutate_metrics(fri_polygons,
#'                                        rast(metrics)[[c("dens", "lor", "qmdbh", "Vmerch_ha")]],
#'                                        fun = "stdev")
#'
#' fri_polygons_metrics
mutate_metrics <- function(x,
                           y,
                           fun = "median"){

  cat(paste0("Mutate ", fun, " value of ", names(y), " for ", nrow(x), " polygon(s)\n"), sep = "")
  x_crs <- sf::st_transform(x, sf::st_crs(y))
  metrics_extracted <- exactextractr::exact_extract(y, x_crs, fun, progress = FALSE)
  names(metrics_extracted) <- paste0(names(y), "_", fun)
  x_metrics <- dplyr::bind_cols(x, metrics_extracted)

  return(x_metrics)
}
