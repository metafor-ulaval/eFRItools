#' Separate a column of species composition into species orders
#'
#' @param x sf; polygons from the `sf` package.
#' @param column_name character; name of the column that contains species and proportions.
#'
#' @returns
#' Polygons with extracted species orders as individual columns
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_order <- mutate_species_order(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species_order
mutate_species_order <- function(x,
                                 column_name){

  cat(paste0("Mutate species order for ", nrow(x), " polygon(s)\n"))
  # List of species with proportion for each polygon, e.g. c("BW  40", "SB  40", "PT  20")
  species <- regmatches(x[[column_name]], gregexpr("[A-Z]{2} +[0-9]+", x[[column_name]]))

  # One row per polygon with the species codes in order, padded with NA
  n_max <- max(lengths(species))
  species_order <- lapply(species, function(xx){ substr(xx, 1, 2)[seq_len(n_max)] })
  species_order <- as.data.frame(do.call(rbind, species_order))
  names(species_order) <- paste0("SP_NO_", seq_len(n_max))

  # Existing species order columns are replaced
  x <- dplyr::select(x, -dplyr::any_of(names(species_order)))
  x <- dplyr::bind_cols(x, species_order)

  return(x)
}
