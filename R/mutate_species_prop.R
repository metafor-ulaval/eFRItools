#' Separate a column of species composition into proportions
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param col_name character; name of the column that contains species and proportions
#' @param update_names logical; if some propportions are already presents, duplicated columns will be updated
#'
#' @returns
#' Polygons with extracted species proportions as individuals columns
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species_prop
mutate_species_prop <- function(x,
                                col_name,
                                update_names = TRUE){

  extracted_col <- dplyr::pull(x, !!rlang::sym(col_name))
  extracted_col_list <- stringr::str_match_all(extracted_col, "[A-Z]{2}[ ]+[0-9]+")
  purrr::map(extracted_col_list,
             ~{tibble::as_tibble(.x, .name_repair = "unique_quiet") |>
                 tidyr::separate(1, into = c("SP", "PROP")) |>
                 dplyr::mutate(PROP = as.numeric(PROP)) |>
                 tidyr::pivot_wider(names_from = SP, values_from = PROP)}) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("NA")) |>
    dplyr::mutate_all(~replace_na(.x, 0)) -> species_prop

  if(update_names == TRUE){
    x <- dplyr::select(x, -dplyr::any_of(names(species_prop)))
  }

  x_species_prop <- dplyr::bind_cols(x, species_prop)

  return(x_species_prop)
}
