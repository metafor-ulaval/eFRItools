#' Extract the proportion of the polygon that are forested
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param y raster; Landcover SpatRaster from the `terra` package.
#' @param forested_class vector; all class of the landcover SpatRaster that are forested
#'
#' @returns
#' Column "PROPFORESTED"
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_prop_forested <- mutate_prop_forested(fri_polygons, rast(landcover), forested_class = c("Sparse_Treed",
#'                                                                                                      "Treed_Upland",
#'                                                                                                      "Deciduous_Treed",
#'                                                                                                      "Mixed_Treed",
#'                                                                                                      "Coniferous_Treed",
#'                                                                                                      "Plantations_Treed_Cultivated",
#'                                                                                                      "Hedge_Rows",
#'                                                                                                      "Tallgrass_Woodland"))
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

  x_crs <- sf::st_transform(x, sf::st_crs(y))
  x_sum_cover <- exactextractr::exact_extract(y, x_crs, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover)

  x_proportion <- purrr::map2_dfr(x_sum_cover,
                                  sf::st_geometry(x_crs),
                                  function(xx,yy){

                                    tibble::tibble(value = NA,
                                                   class_area = sf::st_area(yy) - sum(xx$class_area)) |>
                                      dplyr::add_row(xx) |>
                                      dplyr::mutate(total_area = sf::st_area(yy)) |>
                                      dplyr::mutate(proportion = class_area / total_area * 100) |>
                                      dplyr::select(value, proportion) |>
                                      tidyr::pivot_wider(values_from = proportion, names_from = value)

                                  }) |>
    dplyr::mutate_all(~tidyr::replace_na(.x, 0))

  y_class <- terra::cats(y)[[1]]
  y_class[nrow(y_class) + 1,] <- "NA"
  names(x_proportion) <- y_class$class[match(names(x_proportion), y_class$value)]

  dplyr::select(x_proportion, any_of(forested_class)) |>
    dplyr::reframe(PROPFORESTED = rowSums(dplyr::across(dplyr::everything()))) -> prop_forested

  x_prop_forested <- dplyr::bind_cols(x, prop_forested)

  return(x_prop_forested)
}
