#' Extract disturbance column from existing disturbance columns with a pattern.
#'
#' @param x sf; polygons from the `sf` package. Disturbance proportion columns must be present, see [mutate_proportion()].
#' @param col_name character; names of the columns that contain disturbance proportions.
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
#' col_name <- grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(fri_polygons_proportion), value = TRUE)
#'
#' fri_polygons_disturbance <- mutate_disturbance(fri_polygons_proportion, col_name, 80)
mutate_disturbance <- function(x,
                              col_name,
                              threshold = 80){

  cat(paste0("Mutate disturbance for ", nrow(x), " polygon(s)\n"))
  x_col <- as.data.frame(x)[,col_name]

  x_disturbance <- apply(x_col,
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

  x_disturbance <- do.call(rbind, x_disturbance)

  x[names(x_disturbance)] <- x_disturbance[names(x_disturbance)]

  return(x)
}
