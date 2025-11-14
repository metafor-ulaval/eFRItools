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
#' @param segmentation param
#' @param forest_polygon param
#' @param metrics param
#' @param landcover param
#' @param forest_fire param
#' @param forest_harvest param
#' @param ctg param
#' @param lidar_year_field param
#' @param forest_year_field param
#' @param forest_composition_field param
#' @param forest_type_field param
#' @param target_var param
#' @param knn_var param
#'
#' @returns
#' eFRI_imputation
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

  # Remove forest polygon that have a perturbation between interpretation year and lidar year
  cat("Remove forest polygon that have a perturbation between interpretation year and lidar year\n")
  forest_polygon <- forest_polygon[c(forest_year_field, forest_composition_field, forest_type_field)]
  forest_polygon <- mutate_proportion(forest_polygon, forest_fire, prefix = "forest_fire", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_proportion(forest_polygon, forest_harvest, prefix = "forest_harvest", simplify = FALSE, keep_all = TRUE)
  forest_polygon <- mutate_perturbation(forest_polygon, col_name = grep("^FOREST_FIRE_|^FOREST_HARVEST_", names(forest_polygon), value = TRUE), threshold = 50)
  forest_polygon <- sf::st_intersection(forest_polygon, ctg[lidar_year_field])
  forest_polygon$is_perturbed <- ifelse((forest_polygon[[forest_year_field]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[lidar_year_field]]) |
                                          (forest_polygon[[lidar_year_field]] <= forest_polygon$YRDEP & forest_polygon$YRDEP <= forest_polygon[[forest_year_field]]), 0, 1)
  forest_polygon$is_perturbed[is.na(forest_polygon$is_perturbed)] <- 1
  forest_polygon <- forest_polygon[forest_polygon$is_perturbed != 0,]

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

  # Extract data for imputation into forest polygon
  cat("Extract data for imputation into forest polygon\n")
  forest_polygon <- mutate_centroid(forest_polygon)
  forest_polygon <- mutate_species_prop(forest_polygon, "SPCOMP")
  forest_polygon <- mutate_species_order(forest_polygon, "SPCOMP")
  forest_polygon <- mutate_forest_type(forest_polygon)
  forest_polygon <- mutate_functional_group(forest_polygon)

  # Extract data for imputation into segmentation
  cat("Extract data of segmentation/n")
  segmentation_data <- segmentation
  segmentation_data <- mutate_centroid(segmentation_data)
  segmentation_data <- mutate_metrics(segmentation_data, metrics, fun = "median")

  for(i in seq_along(target_var)){

    cat(paste0("Imputation of variable : ", target_var[i], "\n"))

    knn_var_temp <- strsplit(knn_var[i], ",")[[1]]

    forest_polygon_temp <- forest_polygon[c(knn_var_temp, target_var[i])]
    forest_polygon_temp <- na.omit(forest_polygon_temp)

    segmentation_temp <- segmentation_data[knn_var_temp]
    #segmentation_temp <- na.omit(segmentation_temp) # Join by id if it don't work

    segmentation[target_var[i]] <- knn_inputation(reference_polygons = forest_polygon_temp,
                                                  target_polygons = segmentation_temp,
                                                  knn_variables = knn_var_temp,
                                                  target_variables = target_var[i],
                                                  k = 5)
  }

  return(segmentation)
}
