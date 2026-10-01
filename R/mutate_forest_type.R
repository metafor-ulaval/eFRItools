#' Extract forest type proportions from species proportion columns
#'
#' @param x sf; polygons from the `sf` package. Species proportion columns must be present, see [mutate_species_prop()].
#'
#' @returns
#' Polygons with extracted forest type proportions as two individual columns: PROP_CONIFEROUS and PROP_DECIDUOUS
#' @export
#'
#' @examples
#' library(sf)
#'
#' fri_polygons_species_prop <- mutate_species_prop(fri_polygons["SPCOMP"], "SPCOMP")
#'
#' fri_polygons_forest_type <- mutate_forest_type(fri_polygons_species_prop)
#'
#' fri_polygons_forest_type
mutate_forest_type <- function(x){

  cat(paste0("Mutate forest type for ", nrow(x), " polygon(s)\n"))
  dict <- data.frame(SB = "black spruce",    LA = "eastern larch",       BW = "white birch",     BF = "balsam fir",
                     CE = "cedar",           SW = "white spruce",        PT = "trembling aspen", PJ = "jack pine",
                     PO = "poplar",          PB = "balsam poplar",       PR = "red pine",        PW = "white pine",
                     SX = "spruce",          MR = "red maple",           AB = "black ash",       BY = "yellow birch",
                     OR = 'red oak',         CW = 'eastern white cedar', MH = 'hard maple',      HE = 'eastern hemlock',
                     BD = 'basswood',        CB = 'black cherry',        BE = 'american beech',  AW = 'white ash',
                     PL = 'largetooth aspen',AG = 'red ash',             OW = 'white oak',       IW = 'ironwood',
                     OB = 'bur oak',         EW = 'white elm',           MS = 'silver maple',    PS = 'scots pine',
                     OH = 'other hardwoods', BG = 'grey birch',          AL = 'alder',           SR = 'red spruce',
                     BB = 'blue beech',      MT = 'mountain maple',      MB = 'black maple',     OC = 'other conifers',
                     SN = 'norway spruce',   PE = 'silver poplar',       HI = 'hickory',         AX = 'ash')


  coniferous <- names(dict[grepl("pine|spruce|fir|cedar|larch|conifers|hemlock", dict)])

  deciduous <- names(dict[!grepl("pine|spruce|fir|cedar|larch|conifers|hemlock", dict)])

  x |>
    dplyr::mutate(PROP_CONIFEROUS = rowSums(dplyr::across(dplyr::any_of(coniferous)))) |>
    dplyr::mutate(PROP_DECIDUOUS = rowSums(dplyr::across(dplyr::any_of(deciduous)))) -> x_forest_type

  return(x_forest_type)

}
