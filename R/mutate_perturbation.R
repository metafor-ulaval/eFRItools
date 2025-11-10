#' Extract perturbation column from existing perturbation coloumns with a pettern.
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package. proportion of perturbations columns must be present before running the function. see [mutate_proportion]
#' @param col_name character; name of the column that contains perturbation proportions
#' @param threshold numeric; minimum percentage coverage to consider the perturbation to be significant
#'
#' @returns
#' Polygons with the follow perturbation columns : YRDEP, DEPTYPE, DEPPROP
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' x <- fri_polygons |>
#'   mutate_proportion(rast(forest_fire_1985_2020), "forest_fire") |>
#'   mutate_proportion(rast(forest_harvest_1985_2020), "forest_harvest")
#'
#' col_name <- grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(x), value = TRUE)
#'
#' threshold <- 20
mutate_perturbation <- function(x,
                                col_name,
                                threshold = 80){

  x_col <- as.data.frame(x)[,c(col_name)]

  x_perturbation <- apply(x_col,
                          MARGIN = 1,
                          function(xx){

                            xx_threshold <- xx[xx > threshold]
                            xx_years <- as.numeric(gsub("\\D", "", names(xx_threshold)))
                            xx_max <- xx_threshold[which.max(xx_years)]
                            xx_type <- sub("_?[0-9]+.*", "", names(xx_max))
                            xx_years <- as.numeric(gsub("\\D", "", names(xx_max)))

                            data.frame(YRDEP = xx_years,
                                       DEPTYPE = xx_type,
                                       DEPPROP = xx_max)
                          })

  x_perturbation <- do.call(rbind, x_perturbation)

  x[names(x_perturbation)] <- x_perturbation[names(x_perturbation)]

  return(x)
}

