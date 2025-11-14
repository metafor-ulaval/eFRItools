#' Extract the proportion of each class of a raster in specified polygons
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param y raster; SpatRaster with categorical values from the `terra` package.
#' @param prefix character; prefix use for created column for each class
#' @param remove_class character; class needed to be removed, like na or 0
#' @param simplify logical; compute the most frequent class and its proportion
#' @param keep_all logical; keep all column of individual class
#'
#' @returns
#' Polygons with extracted class proportions as individuals columns
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_landcover_prop <- mutate_proportion(fri_polygons, rast(landcover), "landcover")
#'
#' fri_polygons_landcover_prop
mutate_proportion <- function(x,
                              y,
                              prefix,
                              remove_class = c("NaN" ,"NA", "0"),
                              simplify = TRUE,
                              keep_all = TRUE){

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
    dplyr::mutate_all(~tidyr::replace_na(.x, 0)) |>
    dplyr::select(-dplyr::any_of(remove_class))

  y_class <- terra::cats(y)[[1]]

  if(!is.null(y_class)){

    y_class[nrow(y_class) + 1,] <- "NA"
    names(x_proportion) <- y_class$class[match(names(x_proportion), y_class$value)]

  }

  if(simplify == TRUE){

    x_proportion |>
      tibble::rowid_to_column("id") |>
      tidyr::pivot_longer(!id, names_to = "variable", values_to = "value") |>
      dplyr::group_by(id) |>
      dplyr::arrange(dplyr::desc(value)) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(-id) |>
      dplyr::mutate(variable = toupper(variable)) |>
      dplyr::mutate(variable = ifelse(value == 0, NA, variable)) |>
      dplyr::mutate(value = ifelse(value == 0, NA, value)) -> x_proportion_simplified

    names(x_proportion_simplified) <- c(paste0("most_frequent_", prefix), paste0("most_frequent_", prefix, "_proportion"))
    names(x_proportion) <- paste0(prefix, "_", names(x_proportion), "_proportion")

    x_proportion_results <- dplyr::bind_cols(x_proportion, x_proportion_simplified)

  } else {

    names(x_proportion) <- paste0(prefix, "_", names(x_proportion), "_proportion")

    x_proportion_results <- x_proportion

  }

  if(keep_all == FALSE){

    dplyr::select(x_proportion_results, -names(x_proportion)) -> x_proportion_results

  }

  names(x_proportion_results) <- toupper(names(x_proportion_results))
  x_proportion_results <- dplyr::bind_cols(x, x_proportion_results)

  return(x_proportion_results)

}
