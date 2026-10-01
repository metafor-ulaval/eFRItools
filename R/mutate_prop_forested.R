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

  x_proportion <- purrr::map2_dfr(x_sum_cover,
                                  sf::st_geometry(x_crs),
                                  function(xx,yy){

                                    x_area <- as.numeric(sf::st_area(yy))

                                    x_coverage_area_na <- tibble::tibble(value = NA,
                                                                         coverage_area = x_area - sum(xx$coverage_area))

                                    dplyr::rows_upsert(xx, x_coverage_area_na, by = "value") |>
                                      dplyr::mutate(total_area = x_area) |>
                                      dplyr::mutate(proportion = coverage_area / total_area * 100) |>
                                      dplyr::select(value, proportion) |>
                                      tidyr::pivot_wider(values_from = proportion, names_from = value)

                                  }) |>
    dplyr::mutate_all(~tidyr::replace_na(.x, 0))

  y_class <- terra::cats(y)[[1]]
  y_class[nrow(y_class) + 1,] <- "NA"
  names(x_proportion) <- y_class$class[match(names(x_proportion), y_class$value)]

  dplyr::select(x_proportion, dplyr::any_of(forested_class)) |>
    dplyr::reframe(PROPFORESTED = rowSums(dplyr::across(dplyr::everything()))) -> prop_forested

  x_prop_forested <- dplyr::bind_cols(x, prop_forested)

  return(x_prop_forested)
}
