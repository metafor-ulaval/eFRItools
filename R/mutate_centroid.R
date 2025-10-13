#' Extract the X and Y coordinates of the polygon centroids
#'
#' @param x spatial features; polygons from the `sf` package.
#'
#' @returns
#' Polygons with extracted X and Y coordinates as individuals columns
#' @export
#'
#' @examples
#'library(sf)
#'
#'fri_polygons_centroids <- mutate_centroid(fri_polygons)
#'
#'fri_polygons_centroids
mutate_centroid <- function(x){

  x_geom <- sf::st_geometry(x)
  x_points <- sf::st_centroid(x_geom)
  x_coords <- sf::st_coordinates(x_points)
  x_centroid <- dplyr::bind_cols(x, x_coords)

  return(x_centroid)
}
