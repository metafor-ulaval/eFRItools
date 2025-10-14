#' Separate a column of species composition into multiple columns
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param col_name character; name of the column that contains species and proportions
#'
#' @returns
#' Polygons with extracted specie proportions as individuals columns
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species
mutate_species_prop <- function(x,
                                col_name){

  extracted_col <- dplyr::pull(x, col_name)
  extracted_col_list <- stringr::str_match_all(extracted_col, "[A-Z]{2}[ ]+[0-9]+")
  purrr::map(extracted_col_list,
             ~{tibble::as_tibble(.x, .name_repair = "unique_quiet") |>
                 tidyr::separate(1, into = c("SP", "PROP")) |>
                 dplyr::mutate(PROP = as.numeric(PROP)) |>
                 tidyr::pivot_wider(names_from = SP, values_from = PROP)}) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-dplyr::any_of("NA")) -> species_prop
  x_species_prop <- dplyr::bind_cols(x, species_prop)

  return(x_species_prop)
}
