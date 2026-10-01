#' eFRI_attribute_table
#'
#' @param segmentation sf; polygons from the `sf` package produced by [eFRI_segmentation()].
#' @param metrics SpatRaster; metrics with one layer per metric from the `terra` package.
#' @param landcover SpatRaster; categorical landcover raster from the `terra` package.
#' @param forest_fire SpatRaster; year of forest fire from the `terra` package.
#' @param forest_harvest SpatRaster; year of forest harvest from the `terra` package.
#' @param forest_age SpatRaster; forest age in 2019 from the `terra` package.
#' @param summary_metrics character; names of the `metrics` layers to summarize (median) as columns. `NULL` to skip.
#'
#' @returns
#' Polygons with the eFRI attribute table as columns
#' @export
#'
#' @examples
#' eFRI_attribute_table
eFRI_attribute_table <- function(segmentation,
                                 metrics,
                                 landcover,
                                 forest_fire,
                                 forest_harvest,
                                 forest_age,
                                 summary_metrics = NULL){

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  segmentation_final <- segmentation

  segmentation <- mutate_proportion(segmentation, landcover, prefix = "landcover", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_prop_forested(segmentation, landcover)
  segmentation <- mutate_proportion(segmentation, forest_fire, prefix = "forest_fire", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_proportion(segmentation, forest_harvest, prefix = "forest_harvest", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_age(segmentation, forest_age, 2019, current_year, fun = "mean") # PEUT-ETRE MEDIANE?
  segmentation <- mutate_disturbance(segmentation, column_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(segmentation), value = TRUE), threshold = 80)

  segmentation_final$SOURCE <- "eFRI"
  segmentation_final$YRSOURCE <- current_year
  segmentation_final$AREA <- round(as.numeric(sf::st_area(segmentation)), 2)
  segmentation_final$PERIMETER <- round(as.numeric(sf::st_perimeter(segmentation)), 2)
  segmentation_final$PROPFORESTED <-  round(segmentation$PROPFORESTED, 2)
  segmentation_final$POLYTYPE <- segmentation$MOST_FREQUENT_LANDCOVER
  segmentation_final$POLYTYPEPROP <-  round(segmentation$MOST_FREQUENT_LANDCOVER_PROPORTION, 2)
  segmentation_final$AGE <- segmentation[[paste0("age_mean_", current_year)]]
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
