#' Extract perturbation column from existing perturbation columns with a pattern.
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
#' fri_polygons_proportion <- fri_polygons |>
#'   mutate_proportion(rast(forest_fire_1985_2020), "forest_fire") |>
#'   mutate_proportion(rast(forest_harvest_1985_2020), "forest_harvest")
#'
#' col_name <- grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(fri_polygons_proportion), value = TRUE)
#'
#' fri_polygons_perturbation <- mutate_perturbation(fri_polygons_proportion, col_name, 80)
mutate_perturbation <- function(x,
                                col_name,
                                threshold = 80){

  cat(paste0("Mutate perturbation for ", nrow(x), " polygon(s)\n"))
  x_col <- as.data.frame(x)[,col_name]

  x_perturbation <- apply(x_col,
                          MARGIN = 1,
                          function(xx){

                            xx_temp <- xx[which.max(xx)]
                            xx_temp[xx_temp < threshold] <- NA

                            if(all(is.na(xx_temp))){

                              xx_temp <- data.frame(YRDEP = NA,
                                                    DEPTYPE = NA,
                                                    DEPPROP = NA)

                            } else {

                              xx_temp <- data.frame(YRDEP = as.numeric(gsub("\\D", "", names(xx_temp))),
                                                    DEPTYPE = sub("_?[0-9]+.*", "", names(xx_temp)),
                                                    DEPPROP = as.numeric(xx_temp))
                            }

                            return(xx_temp)

                          })

  x_perturbation <- do.call(rbind, x_perturbation)

  x[names(x_perturbation)] <- x_perturbation[names(x_perturbation)]

  return(x)
}
