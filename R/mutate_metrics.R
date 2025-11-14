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
#' library(sf)
#' library(terra)
#'
#' fri_polygons_metrics <- mutate_metrics(fri_polygons, rast(metrics))
#'
#' fri_polygons_metrics
mutate_metrics <- function(x,
                           y,
                           fun = "median"){

  cat(paste0("Mutate ", fun, " value of ", names(y), " for ", nrow(x), " polygon(s)\n"))
  x_crs <- sf::st_transform(x, sf::st_crs(y))
  metrics_extracted <- exactextractr::exact_extract(y, x_crs, fun)
  names(metrics_extracted) <- names(y)
  x_metrics <- dplyr::bind_cols(x, metrics_extracted)

  return(x_metrics)
}
