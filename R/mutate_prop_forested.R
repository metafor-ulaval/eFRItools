#' Extract the proportion of the polygon that are forested
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; categorical landcover raster from the `terra` package.
#' @param forested_class character; classes of the landcover raster `y` that are forested.
#'
#' @returns
#' Polygons with the forested proportion as a single column: PROPFORESTED
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_prop_forested <- mutate_prop_forested(fri_polygons,
#'                                                    rast(landcover),
#'                                                    forested_class = c("Sparse_Treed",
#'                                                                       "Treed_Upland",
#'                                                                       "Deciduous_Treed",
#'                                                                       "Mixed_Treed",
#'                                                                       "Coniferous_Treed",
#'                                                                       "Plantations_Treed_Cultivated",
#'                                                                       "Hedge_Rows",
#'                                                                       "Tallgrass_Woodland"))
#'
#' fri_polygons_prop_forested
mutate_prop_forested <- function(x,
                                 y,
                                 forested_class = c("Sparse_Treed",
                                                    "Treed_Upland",
                                                    "Deciduous_Treed",
                                                    "Mixed_Treed",
                                                    "Coniferous_Treed",
                                                    "Plantations_Treed_Cultivated",
                                                    "Hedge_Rows",
                                                    "Tallgrass_Woodland")){

  cat(paste0("Mutate proportion forested for ", nrow(x), " polygon(s)\n"))
  x_crs <- sf::st_transform(x, sf::st_crs(y))
  x_sum_cover <- exactextractr::exact_extract(y, x_crs, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover, progress = FALSE)

  # Proportion of each forested class in each polygon (0 when absent), filled in one step
  area <- as.numeric(sf::st_area(x_crs))
  cover <- dplyr::bind_rows(x_sum_cover, .id = "polygon")
  cover <- cover[!is.na(cover$value), , drop = FALSE]
  polygon <- as.integer(cover$polygon)

  y_class <- terra::cats(y)[[1]]
  cover_class <- y_class$class[match(cover$value, y_class$value)]
  forested <- cover_class %in% forested_class

  x_proportion <- matrix(0, nrow = length(x_sum_cover), ncol = length(forested_class))
  x_proportion[cbind(polygon[forested], match(cover_class[forested], forested_class))] <-
    cover$coverage_area[forested] / area[polygon[forested]] * 100

  prop_forested <- tibble::tibble(PROPFORESTED = rowSums(x_proportion))

  x_prop_forested <- dplyr::bind_cols(x, prop_forested)

  return(x_prop_forested)
}
