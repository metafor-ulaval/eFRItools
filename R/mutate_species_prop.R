#' Separate a column of species composition into proportions
#'
#' @param x sf; polygons from the `sf` package.
#' @param column_name character; name of the column that contains species and proportions.
#'
#' @returns
#' Polygons with extracted species proportions as individual columns. Existing species columns are replaced.
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species_prop
mutate_species_prop <- function(x,
                                column_name){

  cat(paste0("Mutate species proportion for ", nrow(x), " polygon(s)\n"))
  # List of species with proportion for each polygon, e.g. c("BW  40", "SB  40", "PT  20")
  species <- regmatches(x[[column_name]], gregexpr("[A-Z]{2} +[0-9]+", x[[column_name]]))

  # One row per species of each polygon
  row <- rep(seq_along(species), lengths(species))
  code <- substr(unlist(species), 1, 2)
  prop <- as.numeric(substring(unlist(species), 3))

  # Table of proportions with one column per species (0 when absent)
  species_prop <- tapply(prop,
                         list(factor(row, levels = seq_along(species)), factor(code, levels = unique(code))),
                         sum,
                         default = 0)
  rownames(species_prop) <- NULL
  species_prop <- as.data.frame(species_prop)
  species_prop <- species_prop[names(species_prop) != "NA"]

  # Existing species columns are replaced
  x <- dplyr::select(x, -dplyr::any_of(names(species_prop)))
  x <- dplyr::bind_cols(x, species_prop)

  return(x)
}
