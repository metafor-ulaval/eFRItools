#' Extract disturbance column from existing disturbance columns with a pattern.
#'
#' @param x sf; polygons from the `sf` package. Disturbance proportion columns must be present, see [mutate_proportion()].
#' @param column_name character; names of the columns that contain disturbance proportions.
#' @param threshold numeric; minimum proportion (%) for a disturbance to be considered significant.
#'
#' @returns
#' Polygons with the following disturbance columns: YRDEP, DEPTYPE and DEPPROP
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_proportion <- fri_polygons |>
#'   mutate_proportion(rast(forest_fire_1985_2020), "forest_fire") |>
#'   mutate_proportion(rast(forest_harvest_1985_2020), "forest_harvest")
#'
#' column_name <- grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(fri_polygons_proportion), value = TRUE)
#'
#' fri_polygons_disturbance <- mutate_disturbance(fri_polygons_proportion, column_name, 80)
mutate_disturbance <- function(x,
                              column_name,
                              threshold = 80){

  cat(paste0("Mutate disturbance for ", nrow(x), " polygon(s)\n"))
  x_col <- as.matrix(as.data.frame(x)[, column_name, drop = FALSE])

  # Largest disturbance of each polygon, kept only when it reaches the threshold
  column <- which_max_row(x_col)
  proportion <- x_col[cbind(seq_along(column), column)]
  significant <- !is.na(proportion) & proportion >= threshold
  column[!significant] <- NA
  proportion[!significant] <- NA
  name <- colnames(x_col)[column]

  x_disturbance <- data.frame(YRDEP = as.numeric(gsub("\\D", "", name)),
                              DEPTYPE = sub("_?[0-9]+.*", "", name),
                              DEPPROP = as.numeric(proportion))

  x[names(x_disturbance)] <- x_disturbance[names(x_disturbance)]

  return(x)
}
