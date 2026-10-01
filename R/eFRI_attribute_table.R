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

  segmentation_final <- segmentation

  segmentation <- mutate_proportion(segmentation, landcover, prefix = "landcover", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_prop_forested(segmentation, landcover)
  segmentation <- mutate_proportion(segmentation, forest_fire, prefix = "forest_fire", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_proportion(segmentation, forest_harvest, prefix = "forest_harvest", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_age(segmentation, forest_age, 2019, lubridate::year(lubridate::today()), fun = "mean") # PEUT-ETRE MEDIANE?
  segmentation <- mutate_disturbance(segmentation, col_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(segmentation), value = TRUE), threshold = 80)

  segmentation_final$SOURCE <- "eFRI"
  segmentation_final$YRSOURCE <- lubridate::year(lubridate::today())
  segmentation_final$AREA <- round(as.numeric(sf::st_area(segmentation)), 2)
  segmentation_final$PERIMETER <- round(as.numeric(sf::st_perimeter(segmentation)), 2)
  segmentation_final$PROPFORESTED <-  round(segmentation$PROPFORESTED, 2)
  segmentation_final$POLYTYPE <- segmentation$MOST_FREQUENT_LANDCOVER
  segmentation_final$POLYTYPEPROP <-  round(segmentation$MOST_FREQUENT_LANDCOVER_PROPORTION, 2)
  segmentation_final$AGE <- segmentation[[paste0("age_mean_", lubridate::year(lubridate::today()))]]
  segmentation_final$YRDEP <- segmentation$YRDEP
  segmentation_final$DEPTYPE <- segmentation$DEPTYPE
  segmentation_final$DEPPROP <- round(segmentation$DEPPROP, 2)
  segmentation_final$YRHARVEST <- if ("MOST_FREQUENT_FOREST_HARVEST" %in% names(segmentation)) {segmentation$MOST_FREQUENT_FOREST_HARVEST} else {NA}
  segmentation_final$HARVESTPROP <- if ("MOST_FREQUENT_FOREST_HARVEST_PROPORTION" %in% names(segmentation)) {round(segmentation$MOST_FREQUENT_FOREST_HARVEST_PROPORTION, 2)} else {NA}
  segmentation_final$YRFIRE <- if ("MOST_FREQUENT_FOREST_FIRE" %in% names(segmentation)) {segmentation$MOST_FREQUENT_FOREST_FIRE} else {NA}
  segmentation_final$FIREPROP <- if ("MOST_FREQUENT_FOREST_FIRE_PROPORTION" %in% names(segmentation)) {round(segmentation$MOST_FREQUENT_FOREST_FIRE_PROPORTION, 2)} else {NA}

  if(!is.null(summary_metrics)){
    segmentation <- mutate_metrics(segmentation, metrics[[summary_metrics]], fun = "median")
    segmentation_final[toupper(summary_metrics)] <- round(sf::st_drop_geometry(segmentation[,paste0(summary_metrics, "_median")]), 2)
    } # Will fail if the summary metric is not "median", correct later

  return(segmentation_final)
}
