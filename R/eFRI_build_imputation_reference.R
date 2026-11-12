#' eFRI_build_imputation_reference
#'
#' Builds the reference polygons used by [eFRI_imputation()] from the Forest Resources Inventory.
#' Polygons disturbed between the inventory year and the LiDAR year, non-forested polygons and
#' polygons with low height or canopy cover are removed. The remaining polygons get the median of
#' each metric, their centroid, their species proportions and order, their forest type and their
#' functional group.
#'
#' The reference only depends on the inventory and on the rasters, not on the segmentation, so it
#' can be built once and saved (e.g. with `sf::st_write()`) to be reused by several imputations.
#'
#' @param forest_polygon sf; reference polygons of Forest Resources Inventory from the `sf` package.
#' @param metrics SpatRaster; metrics with one layer per metric from the `terra` package. Must contain `z_p95`, `z_above2` and the metrics later used as `knn_variables` in [eFRI_imputation()].
#' @param landcover SpatRaster; categorical landcover raster from the `terra` package.
#' @param forest_fire SpatRaster; year of forest fire from the `terra` package.
#' @param forest_harvest SpatRaster; year of forest harvest from the `terra` package.
#' @param ctg sf; LiDAR acquisition tiles from the `sf` package.
#' @param lidar_year_column character; name of the column of `ctg` that contains the LiDAR acquisition year.
#' @param forest_year_column character; name of the column of `forest_polygon` that contains the inventory year.
#' @param forest_composition_column character; name of the column of `forest_polygon` that contains species and proportions.
#' @param forest_type_column character; name of the column of `forest_polygon` that contains the polygon type. Only "FOR" polygons are used.
#'
#' @returns
#' Reference polygons (`sf`) to pass to [eFRI_imputation()]. May have no row when no polygon is kept.
#' @export
#'
#' @examples
#' eFRI_build_imputation_reference
eFRI_build_imputation_reference <- function(forest_polygon,
                                            metrics,
                                            landcover,
                                            forest_fire,
                                            forest_harvest,
                                            ctg,
                                            lidar_year_column,
                                            forest_year_column,
                                            forest_composition_column,
                                            forest_type_column){

  # Remove forest polygon that have a disturbance between interpretation year and lidar year
  cat("Remove forest polygon that have a disturbance between interpretation year and lidar year\n")
  forest_polygon <- forest_polygon[c(forest_year_column, forest_composition_column, forest_type_column)]
  forest_polygon <- mutate_proportion(forest_polygon, forest_fire, prefix = "forest_fire", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_proportion(forest_polygon, forest_harvest, prefix = "forest_harvest", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_disturbance(forest_polygon, column_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(forest_polygon), value = TRUE), threshold = 50)
  forest_polygon <- sf::st_join(forest_polygon, ctg[lidar_year_column], largest = TRUE)
  forest_polygon$is_disturbed <- ifelse((forest_polygon[[forest_year_column]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[lidar_year_column]]) |
                                          (forest_polygon[[lidar_year_column]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[forest_year_column]]), 1, 0)
  forest_polygon$is_disturbed[is.na(forest_polygon$is_disturbed)] <- 0
  forest_polygon <- forest_polygon[forest_polygon$is_disturbed == 0,]

  # Extract data for filtering
  cat("Extract data of forest polygon\n")
  forest_polygon <- mutate_metrics(forest_polygon, metrics, fun = "median")
  forest_polygon <- mutate_prop_forested(forest_polygon, landcover)

  # Filter non forested forest polygon (which() drops polygons with NA values)
  cat("Filter non forested forest polygon\n")
  forest_polygon <- forest_polygon[which(forest_polygon[[forest_type_column]] == "FOR"),]
  forest_polygon <- forest_polygon[which(forest_polygon$PROPFORESTED >= 50),]
  forest_polygon <- forest_polygon[which(forest_polygon$z_p95_median >= 5),]
  forest_polygon <- forest_polygon[which(forest_polygon$z_above2_median >= 50),]

  if(nrow(forest_polygon) != 0){

    # Extract data for imputation into forest polygon
    cat("Extract data for imputation into forest polygon\n")
    forest_polygon <- mutate_centroid(forest_polygon)
    forest_polygon <- mutate_species_prop(forest_polygon, forest_composition_column)
    forest_polygon <- mutate_species_order(forest_polygon, forest_composition_column)
    forest_polygon <- mutate_forest_type(forest_polygon)
    forest_polygon <- mutate_functional_group(forest_polygon)

  }

  return(forest_polygon)

}
