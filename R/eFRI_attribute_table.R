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
#' @param summary_metrics param
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
                                 summary_metrics,
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
  segmentation_final$AREA <- round(segmentation$AREA, 2)
  segmentation_final$PERIMETER <- round(segmentation$PERIMETER, 2)
  segmentation_final$PROPFORESTED <-  round(segmentation$PROPFORESTED, 2)
  segmentation_final$POLYTYPE <- segmentation$MOST_FREQUENT_LANDCOVER
  segmentation_final$POLYTYPEPROP <-  round(segmentation$MOST_FREQUENT_LANDCOVER_PROPORTION, 2)
  #segmentation_final$YRORG <- segmentation$ # On garde ca?
  segmentation_final$AGE <- segmentation[[paste0("age_mean_", lubridate::year(lubridate::today()))]]
  segmentation_final$YRDEP <- segmentation$YRDEP
  segmentation_final$DEPTYPE <- segmentation$DEPTYPE
  segmentation_final$DEPPROP <- round(segmentation$DEPPROP, 2)
  segmentation_final$YRHARVEST <- segmentation$MOST_FREQUENT_FOREST_HARVEST
  segmentation_final$HARVESTPROP <- round(segmentation$MOST_FREQUENT_FOREST_HARVEST_PROPORTION, 2)
  segmentation_final$YRFIRE <- segmentation$MOST_FREQUENT_FOREST_FIRE
  segmentation_final$FIREPROP <- round(segmentation$MOST_FREQUENT_FOREST_FIRE_PROPORTION, 2)
  if(!is.null(summary_metrics)){    segmentation_final[toupper(summary_metrics)] <- round(sf::st_drop_geometry(segmentation[,summary_metrics]), 2)    }
  segmentation_final$HEIGHT <- round(segmentation$z_p95, 2)
  segmentation_final$CANOPY_COVER <- round(segmentation$z_above2, 2)
  segmentation_final$DENSITY <- round(segmentation$fractional_cover_05_2, 2)
  segmentation_final$SLOPE <- round(segmentation$slope, 2)
  segmentation_final$MOISTURE <- round(segmentation$sagawi, 2)
  segmentation_final$LEADSP <- segmentation$SP_NO_1
  segmentation_final$SECSP <- segmentation$SP_NO_2
  segmentation_final$FUNCTIONAL_GROUP_3 <- segmentation$FUNCTIONAL_GROUP_3
  segmentation_final$FUNCTIONAL_GROUP_5 <- segmentation$FUNCTIONAL_GROUP_5
  sf::st_geometry(segmentation_final) <- "geometry"

  return(segmentation_final)
}
