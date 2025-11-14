#' Separate a column of species composition into species orders
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param col_name character; name of the column that contains species and proportions
#'
#' @returns
#' Polygons with extracted species orders as individuals columns
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_order <- mutate_species_order(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species_order
mutate_species_order <- function(x,
                                 col_name){

  cat(paste0("Mutate species order for ", nrow(x), " polygon(s)\n"))
  extracted_col <- dplyr::pull(x, col_name)
  extracted_col_list <- stringr::str_match_all(extracted_col, "[A-Z]{2}[ ]+[0-9]+")
  purrr::map(extracted_col_list,
             ~{tibble::as_tibble(.x, .name_repair = "unique_quiet") |>
                 tidyr::separate(1, into = c("SP", "PROP")) |>
                 tibble::rowid_to_column("no") |>
                 dplyr::select(-PROP) |>
                 tidyr::pivot_wider(names_from = no, names_glue = "SP_NO_{no}", values_from = SP)}) |>
    dplyr::bind_rows() -> species_order
  x_species_order <- dplyr::bind_cols(x, species_order)

  return(x_species_order)
}
