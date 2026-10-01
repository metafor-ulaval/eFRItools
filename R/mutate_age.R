#' Extract age of forest stand
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; forest age from the `terra` package.
#' @param current_year numeric; year of the forest age raster `y`.
#' @param target_year numeric; year for which the age is computed, usually the current year.
#' @param fun character; summary function of `exactextractr::exact_extract()` (e.g. "mean", "median").
#'
#' @returns
#' Polygons with extracted age as a single column
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_age <- mutate_age(fri_polygons, rast(forest_age_2019), 2019, 2025, "mean")
#'
#' fri_polygons_age
mutate_age <- function(x,
                       y,
                       current_year,
                       target_year,
                       fun){

  cat(paste0("Mutate age for ", nrow(x), " polygon(s) for year ", target_year, " \n"))
  x_crs <- sf::st_transform(x, sf::st_crs(y))
  age_extracted <- exactextractr::exact_extract(y, x_crs, fun, progress = FALSE)
  age_corrected <- (round(age_extracted, 0) + (target_year - current_year))
  x_age <- dplyr::mutate(x, !!paste0("age_", fun, "_", target_year) := age_corrected)

  return(x_age)
}
