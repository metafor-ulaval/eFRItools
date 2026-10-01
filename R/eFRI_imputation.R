# segmentation <- st_read("D:/00_Ontario_eFRI/Test_Shiny/grm1.shp")
# forest_polygon <- fri_polygons
# metrics <- rast(metrics)
# landcover <- rast(landcover)
# forest_fire <- rast(forest_fire_1985_2020)
# forest_harvest <- rast(forest_harvest_1985_2020)
# "D:/00_Ontario_eFRI/RMF" -> wd
# ctg <- st_read(paste0(wd, "/ctg/ctg.shp")) %>% st_transform("EPSG:2958")
# lidar_year_field <- "Fl_Cr_Y"
# forest_year_field <- "YRUPD"
# forest_composition_field <- "SPCOMP"
# forest_type_field <- "POLYTYPE"
# best_models <- read.csv(paste0(wd, "/results/best_model.csv"))
# target_var <- best_models$target_var
# knn_var <- best_models$knn_vars





#' eFRI_imputation
#'
#' @param segmentation sf; polygons from the `sf` package produced by [eFRI_segmentation()] or [eFRI_attribute_table()].
#' @param forest_polygon sf; reference polygons of Forest Resources Inventory from the `sf` package.
#' @param metrics SpatRaster; metrics with one layer per metric from the `terra` package. Must contain `z_p95` and `z_above2`.
#' @param landcover SpatRaster; categorical landcover raster from the `terra` package.
#' @param forest_fire SpatRaster; year of forest fire from the `terra` package.
#' @param forest_harvest SpatRaster; year of forest harvest from the `terra` package.
#' @param ctg sf; LiDAR acquisition tiles from the `sf` package.
#' @param lidar_year_field character; name of the column of `ctg` that contains the LiDAR acquisition year.
#' @param forest_year_field character; name of the column of `forest_polygon` that contains the inventory year.
#' @param forest_composition_field character; name of the column of `forest_polygon` that contains species and proportions.
#' @param forest_type_field character; name of the column of `forest_polygon` that contains the polygon type. Only "FOR" polygons are used.
#' @param target_var character; names of the columns of `forest_polygon` to impute.
#' @param knn_var character; comma-separated metric names used for the knn of each `target_var`, of the same length as `target_var`.
#'
#' @returns
#' `segmentation` with imputed variables as individual columns
#' @export
#'
#' @examples
#' eFRI_imputation
eFRI_imputation <- function(segmentation,
                            forest_polygon,
                            metrics,
                            landcover,
                            forest_fire,
                            forest_harvest,
                            ctg,
                            lidar_year_field,
                            forest_year_field,
                            forest_composition_field,
                            forest_type_field,
                            target_var,
                            knn_var){

  # Remove forest polygon that have a disturbance between interpretation year and lidar year
  cat("Remove forest polygon that have a disturbance between interpretation year and lidar year\n")
  forest_polygon <- forest_polygon[c(forest_year_field, forest_composition_field, forest_type_field)]
  forest_polygon <- mutate_proportion(forest_polygon, forest_fire, prefix = "forest_fire", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_proportion(forest_polygon, forest_harvest, prefix = "forest_harvest", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_disturbance(forest_polygon, col_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(forest_polygon), value = TRUE), threshold = 50)
  forest_polygon <- sf::st_join(forest_polygon, ctg[lidar_year_field], largest = TRUE)
  forest_polygon$is_disturbed <- ifelse((forest_polygon[[forest_year_field]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[lidar_year_field]]) |
                                          (forest_polygon[[lidar_year_field]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[forest_year_field]]), 1, 0)
  forest_polygon$is_disturbed[is.na(forest_polygon$is_disturbed)] <- 0
  forest_polygon <- forest_polygon[forest_polygon$is_disturbed == 0,]

  # Extract data for filtering
  cat("Extract data of forest polygon\n")
  forest_polygon <- mutate_metrics(forest_polygon, metrics, fun = "median")
  forest_polygon <- mutate_prop_forested(forest_polygon, landcover)

  # Filter non forested forest polygon
  cat("Filter non forested forest polygon\n")
  forest_polygon <- forest_polygon[forest_polygon[[forest_type_field]] == "FOR",]
  forest_polygon <- forest_polygon[forest_polygon$PROPFORESTED  >= 50,]
  forest_polygon <- forest_polygon[forest_polygon$z_p95 >= 5,]
  forest_polygon <- forest_polygon[forest_polygon$z_above2 >= 50,]

  if(nrow(forest_polygon) != 0){

    # Extract data for imputation into forest polygon
    cat("Extract data for imputation into forest polygon\n")
    forest_polygon <- mutate_centroid(forest_polygon)
    forest_polygon <- mutate_species_prop(forest_polygon, "SPCOMP")
    forest_polygon <- mutate_species_order(forest_polygon, "SPCOMP")
    forest_polygon <- mutate_forest_type(forest_polygon)
    forest_polygon <- mutate_functional_group(forest_polygon)

    # Extract data for imputation into segmentation
    cat("Extract data of segmentation\n")
    segmentation$id <- 1:nrow(segmentation)
    segmentation_data <- segmentation
    segmentation_data <- mutate_centroid(segmentation_data)
    segmentation_data <- mutate_metrics(segmentation_data, metrics, fun = "median")

    # Perform imputation
    for(i in seq_along(target_var)){

      cat(paste0("Imputation of variable : ", target_var[i], "\n"))

      knn_var_temp <- strsplit(knn_var[i], ",")[[1]]

      knn_var_temp <- c(paste0(knn_var_temp[knn_var_temp != "X" & knn_var_temp != "Y"], "_median"),
                        knn_var_temp[knn_var_temp == "X" | knn_var_temp == "Y"])

      forest_polygon_temp <- forest_polygon[c(knn_var_temp, target_var[i])]
      forest_polygon_temp <- stats::na.omit(forest_polygon_temp)

      segmentation_temp <- segmentation_data[c("id", knn_var_temp)]
      segmentation_temp <- stats::na.omit(segmentation_temp)

      segmentation_temp[target_var[i]] <- knn_inputation(reference_polygons = forest_polygon_temp,
                                                         target_polygons = segmentation_temp,
                                                         knn_variables = knn_var_temp,
                                                         target_variables = target_var[i],
                                                         k = 5)
      segmentation <- merge(segmentation,
                            sf::st_drop_geometry(segmentation_temp[c("id", target_var[i])]),
                            by = "id",
                            all = TRUE)
    }

  } else {

    cat("No polygon available for imputation\n")

  }

  return(segmentation)

}
