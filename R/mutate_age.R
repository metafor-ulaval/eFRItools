#' Extract age of forest stand
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; forest age from the `terra` package.
#' @param raster_year numeric; year of the forest age raster `y`.
#' @param target_year numeric; year for which the age is computed, usually the current year.
#' @param fun character; summary function of `exactextractr::exact_extract()` (e.g. "median", "mean").
#' @param disturbance SpatRaster; year of the last stand-replacing disturbance (0 or `NA` when none) from the `terra` package.
#'   When given, disturbed pixels take the time since disturbance (`target_year` minus the disturbance year) instead of the age of `y`.
#'   `NULL` to use `y` only.
#'
#' @returns
#' Polygons with extracted age as a single column
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_age <- mutate_age(fri_polygons, rast(forest_age_2019), 2019, 2025, "median")
#'
#' fri_polygons_age
#'
#' # Give priority to the disturbance layers
#' disturbance <- max(rast(forest_fire_1985_2020), rast(forest_harvest_1985_2020))
#' fri_polygons_age <- mutate_age(fri_polygons, rast(forest_age_2019), 2019, 2025, "median",
#'                                disturbance = disturbance)
#'
#' fri_polygons_age
mutate_age <- function(x,
                       y,
                       raster_year,
                       target_year,
                       fun = "median",
                       disturbance = NULL){

  cat(paste0("Mutate age for ", nrow(x), " polygon(s) for year ", target_year, " \n"))

  # Age of each pixel at target_year
  age <- y + (target_year - raster_year)

  if(!is.null(disturbance)){
    if(!terra::compareGeom(disturbance, y, stopOnError = FALSE)){
      disturbance <- terra::project(disturbance, y, method = "near")
    }
    age <- terra::ifel(!is.na(disturbance) & disturbance > 0, target_year - disturbance, age)
  }

  x_crs <- sf::st_transform(x, sf::st_crs(y))
  age_extracted <- exactextractr::exact_extract(age, x_crs, fun, progress = FALSE)
  x[[paste0("age_", fun, "_", target_year)]] <- round(age_extracted, 0)

  return(x)
}
