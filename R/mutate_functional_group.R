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

  cat(paste0("Mutate functional group for ", nrow(x), " polygon(s)\n"))
  x_functional_group <- x

  if(functional_group_3 == TRUE){
    x_functional_group$FUNCTIONAL_GROUP_3 <- "Mixedwood"
    x_functional_group$FUNCTIONAL_GROUP_3[which(x_functional_group[["PROP_DECIDUOUS"]] >= 70)] <- "Hardwood"
    x_functional_group$FUNCTIONAL_GROUP_3[which(x_functional_group[["PROP_CONIFEROUS"]] >= 70)] <- "Softwood"
  }
  if(functional_group_5 == TRUE){
    x_functional_group$FUNCTIONAL_GROUP_5 <- "Mixed Conifer"
    x_functional_group$FUNCTIONAL_GROUP_5[which(x_functional_group[["PROP_DECIDUOUS"]] >= 30 & x_functional_group[["PROP_DECIDUOUS"]] <= 70 & x_functional_group[["PROP_CONIFEROUS"]] >= 30 & x_functional_group[["PROP_CONIFEROUS"]] <= 70)] <- "Mixedwood"
    x_functional_group$FUNCTIONAL_GROUP_5[which(x_functional_group[["PROP_DECIDUOUS"]] >= 70)] <- "Hardwood"
    x_functional_group$FUNCTIONAL_GROUP_5[which(x_functional_group[["SP_NO_1"]] == "SB" & x_functional_group[["SB"]] >= 50 & x_functional_group[["PROP_CONIFEROUS"]] >= 70)] <- "Black Spruce Dominated"
    x_functional_group$FUNCTIONAL_GROUP_5[which(x_functional_group[["SP_NO_1"]] == "PJ" & x_functional_group[["PJ"]] >= 50 & x_functional_group[["PROP_CONIFEROUS"]] >= 70)] <- "Jack Pine Dominated"
  }

  return(x_functional_group)
}
