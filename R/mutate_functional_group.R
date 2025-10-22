#' Extract functional group of stand forest
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package. species codes with prop, order and forest type must be present before running the function. see [mutate_species_prop], [mutate_species_order] and [mutate_forest_type]
#' @param functional_group_3 logical; compute functional group 3 : Softwood, Hardwood and Mixedwood
#' @param functional_group_5 logical; compute functional group 3 : Jack Pine Dominated, Black Spruce Dominated, Softwood, Hardwood and Mixedwood
#'
#' @returns
#' Functionnal group
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_species_order <- mutate_species_order(fri_polygons_species_prop, "SPCOMP")
#'
#' fri_polygons_forest_type <- mutate_forest_type(fri_polygons_species_order)
#'
#' fri_polygons_functional_group <- mutate_functional_group(fri_polygons_forest_type)
#'
#' fri_polygons_functional_group
mutate_functional_group <- function(x,
                                      functional_group_3 = TRUE,
                                      functional_group_5 = TRUE){

  x_functional_group <- x

  if(functional_group_3 == TRUE){

    x_functional_group <- dplyr::mutate(x_functional_group,
                                          FUNCTIONAL_GROUP_3 = dplyr::case_when(PROP_CONIFEROUS >= 70 ~ "Softwood",
                                                                                PROP_DECIDUOUS >= 70 ~ "Hardwood",
                                                                                TRUE ~ "Mixedwood"))

  }

  if(functional_group_5 == TRUE){

    x_functional_group <- dplyr::mutate(x_functional_group,
                                          FUNCTIONAL_GROUP_5 = dplyr::case_when(SP_NO_1 == "PJ" & PJ >= 50 & PROP_CONIFEROUS >= 70 ~ "Jack Pine Dominated",
                                                                                SP_NO_1 == "SB" & SB >= 50 & PROP_CONIFEROUS >= 70 ~ "Black Spruce Dominated",
                                                                                PROP_DECIDUOUS >= 70 ~ "Hardwood",
                                                                                PROP_DECIDUOUS >= 30 & PROP_DECIDUOUS <= 70 & PROP_CONIFEROUS >= 30 & PROP_CONIFEROUS <= 70 ~ "Mixedwood",
                                                                                TRUE ~ "Mixed Conifer"))

  }


  return(x_functional_group)
}
