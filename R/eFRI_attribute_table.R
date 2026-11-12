#' eFRI_attribute_table
#'
#' @param segmentation sf; polygons from the `sf` package produced by [eFRI_segmentation()]. The columns given in `summary_metrics`
#' must already be extracted, e.g. with [mutate_metrics()].
#' @param summary_metrics character; names of the columns of `segmentation` copied (rounded to 2 decimals) into the attribute table
#' (e.g. "z_p95_median"). The names of the vector give the names of the attribute table columns (e.g. `c(HEIGHT = "z_p95_median")`);
#' an unnamed column keeps its name in upper case. `NULL` to skip.
#' @param landcover SpatRaster; categorical landcover raster from the `terra` package.
#' @param forest_fire SpatRaster; year of forest fire from the `terra` package.
#' @param forest_harvest SpatRaster; year of forest harvest from the `terra` package.
#' @param forest_age SpatRaster; forest age at `raster_year` from the `terra` package. Pixels burned or harvested take the time since that disturbance instead (see [mutate_age()]).
#' @param raster_year numeric; reference year of the `forest_age` raster (e.g. 2022 for the NTEMS 2022 forest age).
#' @param target_year numeric; year the ages are brought forward to, also written as `YRSOURCE`. Defaults to the current year.
#' @param age_fun character; summary function of `exactextractr::exact_extract()` used for the polygon age (e.g. "median", "mean").
#'
#' @returns
#' Polygons with the eFRI attribute table as columns
#' @export
#'
#' @examples
#' eFRI_attribute_table
eFRI_attribute_table <- function(segmentation,
                                 summary_metrics = NULL,
                                 landcover,
                                 forest_fire,
                                 forest_harvest,
                                 forest_age,
                                 raster_year,
                                 target_year = as.numeric(format(Sys.Date(), "%Y")),
                                 age_fun = "median"){

  missing <- setdiff(summary_metrics, names(segmentation))
  if(length(missing) > 0){
    stop("Columns missing in segmentation: ", paste(missing, collapse = ", "),
         ". Extract them first with mutate_metrics().")
  }

  segmentation_final <- segmentation

  segmentation <- mutate_proportion(segmentation, landcover, prefix = "landcover", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_prop_forested(segmentation, prefix = "landcover")
  segmentation <- mutate_proportion(segmentation, forest_fire, prefix = "forest_fire", simplify = TRUE, keep_all = TRUE)
  segmentation <- mutate_proportion(segmentation, forest_harvest, prefix = "forest_harvest", simplify = TRUE, keep_all = TRUE)
  if(!terra::compareGeom(forest_harvest, forest_fire, stopOnError = FALSE)){
    forest_harvest <- terra::project(forest_harvest, forest_fire, method = "near")
  }
  disturbance <- max(forest_fire, forest_harvest, na.rm = TRUE) # Year of the last fire or harvest
  segmentation <- mutate_age(segmentation, forest_age, raster_year, target_year, fun = age_fun, disturbance = disturbance)
  segmentation <- mutate_disturbance(segmentation, column_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(segmentation), value = TRUE), threshold = 80)

  segmentation_final$SOURCE <- "eFRI"
  segmentation_final$YRSOURCE <- target_year
  segmentation_final$AREA <- round(as.numeric(sf::st_area(segmentation)), 2)
  segmentation_final$PERIMETER <- round(as.numeric(sf::st_perimeter(segmentation)), 2)
  segmentation_final$PROPFORESTED <-  round(segmentation$PROPFORESTED, 2)
  segmentation_final$POLYTYPE <- segmentation$MOST_FREQUENT_LANDCOVER
  segmentation_final$POLYTYPEPROP <-  round(segmentation$MOST_FREQUENT_LANDCOVER_PROPORTION, 2)
  segmentation_final$AGE <- segmentation[[paste0("age_", age_fun, "_", target_year)]]
  segmentation_final$YRDEP <- segmentation$YRDEP
  segmentation_final$DEPTYPE <- segmentation$DEPTYPE
  segmentation_final$DEPPROP <- round(segmentation$DEPPROP, 2)
  segmentation_final$YRHARVEST <- if ("MOST_FREQUENT_FOREST_HARVEST" %in% names(segmentation)) {segmentation$MOST_FREQUENT_FOREST_HARVEST} else {NA}
  segmentation_final$HARVESTPROP <- if ("MOST_FREQUENT_FOREST_HARVEST_PROPORTION" %in% names(segmentation)) {round(segmentation$MOST_FREQUENT_FOREST_HARVEST_PROPORTION, 2)} else {NA}
  segmentation_final$YRFIRE <- if ("MOST_FREQUENT_FOREST_FIRE" %in% names(segmentation)) {segmentation$MOST_FREQUENT_FOREST_FIRE} else {NA}
  segmentation_final$FIREPROP <- if ("MOST_FREQUENT_FOREST_FIRE_PROPORTION" %in% names(segmentation)) {round(segmentation$MOST_FREQUENT_FOREST_FIRE_PROPORTION, 2)} else {NA}

  if(length(summary_metrics) > 0){
    metrics_names <- names(summary_metrics)
    if(is.null(metrics_names)) metrics_names <- rep("", length(summary_metrics))
    metrics_names[metrics_names == ""] <- toupper(summary_metrics[metrics_names == ""])
    segmentation_final[metrics_names] <- round(sf::st_drop_geometry(segmentation)[unname(summary_metrics)], 2)
  }

  return(segmentation_final)
}
