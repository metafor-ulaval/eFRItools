# x <- segmentation
# y <- landcover
#
#
#
# segmentation <- st_read("D:/00_Ontario_eFRI/Test_Shiny/grm1.shp")
# metrics <- rast(metrics)
# landcover <- rast(landcover)
# forest_fire <- rast(forest_fire_1985_2020)
# forest_harvest <- rast(forest_harvest_1985_2020)
# forest_age <- rast(forest_age_2019)
#
# forest_fire %>% plot
# forest_harvest %>% plot
# metrics[[1]] %>% plot




#' eFRI_attribute_table
#'
#' @param segmentation param
#' @param metrics param
#' @param selected_dendrometrics param
#' @param landcover param
#' @param forest_fire param
#' @param forest_harvest param
#' @param forest_age param
#'
#' @returns
#' eFRI_attribute_table
#' @export
#'
#' @examples
#' eFRI_attribute_table
eFRI_attribute_table <- function(segmentation,
                                 metrics,
                                 selected_dendrometrics,
                                 landcover,
                                 forest_fire,
                                 forest_harvest,
                                 forest_age){

  segmentation$SOURCE <- "eFRI"
  segmentation$YRSOURCE <- lubridate::year(lubridate::today())
  segmentation$AREA <- as.numeric(sf::st_area(segmentation))
  segmentation$PERIMETER <- as.numeric(sf::st_perimeter(segmentation))
  segmentation <- mutate_metrics(segmentation, metrics, fun = "median")
  segmentation <- mutate_proportion(segmentation, landcover, prefix = "landcover", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_prop_forested(segmentation, landcover)
  segmentation <- mutate_proportion(segmentation, forest_fire, prefix = "forest_fire", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_proportion(segmentation, forest_harvest, prefix = "forest_harvest", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_age(segmentation, forest_age, 2019, lubridate::year(lubridate::today()), fun = "mean") # PEUT-ETRE MEDIANE?
  segmentation <- mutate_perturbation(segmentation, col_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(segmentation), value = TRUE), threshold = 80)

  segmentation_final <- sf::st_geometry(segmentation)
  segmentation_final <- sf::st_as_sf(segmentation_final)
  segmentation_final$SOURCE <- segmentation$SOURCE
  segmentation_final$YRSOURCE <- segmentation$YRSOURCE
  segmentation_final$AREA <- segmentation$AREA
  segmentation_final$PERIMETER <- segmentation$PERIMETER
  segmentation_final$PROPFORESTED <- segmentation$PROPFORESTED
  segmentation_final$POLYTYPE <- segmentation$MOST_FREQUENT_LANDCOVER
  segmentation_final$POLYTYPEPROP <- segmentation$MOST_FREQUENT_LANDCOVER_PROPORTION
  #segmentation_final$YRORG <- segmentation$ # On garde ca?
  segmentation_final$AGE <- segmentation$age_mean_2025
  segmentation_final$YRDEP <- segmentation$YRDEP
  segmentation_final$DEPTYPE <- segmentation$DEPTYPE
  segmentation_final$DEPPROP <- segmentation$DEPPROP
  segmentation_final$YRHARVEST <- segmentation$MOST_FREQUENT_FOREST_HARVEST
  segmentation_final$HARVESTPROP <- segmentation$MOST_FREQUENT_FOREST_HARVEST_PROPORTION
  segmentation_final$YRFIRE <- segmentation$MOST_FREQUENT_FOREST_FIRE
  segmentation_final$FIREPROP <- segmentation$MOST_FREQUENT_FOREST_FIRE_PROPORTION
  segmentation_final[toupper(selected_dendrometrics)] <- sf::st_drop_geometry(segmentation[,selected_dendrometrics])
  segmentation_final$HEIGHT <- segmentation$z_p95
  segmentation_final$CANOPY_COVER <- segmentation$z_above2
  segmentation_final$DENSITY <- segmentation$fractional_cover_05_2
  segmentation_final$SLOPE <- segmentation$slope
  segmentation_final$MOISTURE <- segmentation$sagawi
  segmentation_final$LEADSP <- segmentation$SP_NO_1
  segmentation_final$SECSP <- segmentation$SP_NO_2
  segmentation_final$FUNCTIONAL_GROUP_3 <- segmentation$FUNCTIONAL_GROUP_3
  segmentation_final$FUNCTIONAL_GROUP_5 <- segmentation$FUNCTIONAL_GROUP_5
  sf::st_geometry(segmentation_final) <- "geometry"

  return(segmentation_final)
}
