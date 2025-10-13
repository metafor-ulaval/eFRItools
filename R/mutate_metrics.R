#' Extract one or multiple metrics from polygons
#'
#' @param x spatial features; polygons from the `sf` package.
#' @param y metrics; SpatRaster with one or more layers from the `terra` package.
#' @param fun function to summarize the extracted metrics by polygon.
#'
#' @returns
#' Polygons with extracted metrics as individuals columns
#' @export
#'
#' @examples
#'library(sf)
#'library(terra)
#'
#'fri_polygons_geometry <- st_geometry(fri_polygons)
#'
#'fri_polygons_metrics <- mutate_metrics(fri_polygons_geometry, rast(metrics))
#'
#'fri_polygons_metrics
mutate_metrics <- function(x,
                           y,
                           fun = "median"){

  x_crs <- sf::st_transform(x, sf::st_crs(y))

  exactextractr::exact_extract(y, x_crs, fun) -> metrics_extracted

  names(metrics_extracted) = names(y)

  dplyr::bind_cols(x, metrics_extracted) -> x_metrics

  return(x_metrics)
}
