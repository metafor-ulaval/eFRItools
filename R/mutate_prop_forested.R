#' Compute the proportion of the polygon that are forested
#'
#' The forested proportion is the sum of the class proportions already added to `x` by
#' [mutate_proportion()] (with `keep_all = TRUE`), so the landcover raster is not extracted again.
#'
#' @param x sf; polygons from the `sf` package with the class proportion columns of [mutate_proportion()].
#' @param prefix character; prefix given to [mutate_proportion()] (e.g. "landcover" for `LANDCOVER_SPARSE_TREED_PROPORTION`).
#' @param forested_class character; classes of the landcover raster that are forested.
#'
#' @returns
#' Polygons with the forested proportion as a single column: PROPFORESTED
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' # 1. Proportion of each landcover class in each polygon (LANDCOVER_<CLASS>_PROPORTION columns)
#' fri_polygons_landcover_prop <- mutate_proportion(fri_polygons,
#'                                                  rast(landcover),
#'                                                  prefix = "landcover",
#'                                                  simplify = FALSE,
#'                                                  keep_all = TRUE)
#'
#' grep("^LANDCOVER_", names(fri_polygons_landcover_prop), value = TRUE)
#'
#' # 2. Sum of the forested class proportions, without extracting the landcover again
#' fri_polygons_prop_forested <- mutate_prop_forested(fri_polygons_landcover_prop,
#'                                                    prefix = "landcover",
#'                                                    forested_class = c("Sparse_Treed",
#'                                                                       "Treed_Upland",
#'                                                                       "Deciduous_Treed",
#'                                                                       "Mixed_Treed",
#'                                                                       "Coniferous_Treed",
#'                                                                       "Plantations_Treed_Cultivated",
#'                                                                       "Hedge_Rows",
#'                                                                       "Tallgrass_Woodland"))
#'
#' fri_polygons_prop_forested["PROPFORESTED"]
mutate_prop_forested <- function(x,
                                 prefix,
                                 forested_class = c("Sparse_Treed",
                                                    "Treed_Upland",
                                                    "Deciduous_Treed",
                                                    "Mixed_Treed",
                                                    "Coniferous_Treed",
                                                    "Plantations_Treed_Cultivated",
                                                    "Hedge_Rows",
                                                    "Tallgrass_Woodland")){

  cat(paste0("Mutate proportion forested for ", nrow(x), " polygon(s)\n"))

  # Proportion of each forested class in each polygon (0 when absent from the polygon or from all polygons)
  columns <- toupper(paste0(prefix, "_", forested_class, "_proportion"))
  present <- columns %in% names(x)
  x_proportion <- matrix(0, nrow = nrow(x), ncol = length(columns))
  x_proportion[, present] <- as.matrix(sf::st_drop_geometry(x)[columns[present]])
  x_proportion[is.na(x_proportion)] <- 0

  prop_forested <- tibble::tibble(PROPFORESTED = rowSums(x_proportion))

  x_prop_forested <- dplyr::bind_cols(x, prop_forested)

  return(x_prop_forested)
}
