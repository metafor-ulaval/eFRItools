## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

# Where data-raw can be found ----
"D:/00_Ontario_eFRI/RMF" -> wd



# Library ----
library(tidyverse)





# Parameters ----
epsg <- "EPSG:2958"
otb_dir <- "D:/00_Ontario_eFRI/00_logiciels/OTB-9.1.0-Win64/bin"

segmentation_metrics <- c("z_p95", "z_cv", "z_above2", "sagawi", "fractional_cover_05_2")

selected_dendrometrics <- c("Vmerch_ha", "qmdbh", "dens", "lor", "ba_ha")

read.csv(paste0(wd, "/results/best_model.csv")) %>%
  dplyr::pull(knn_vars) %>%
  strsplit(",") %>%
  unlist() %>%
  unique() -> best_models_variables



# Subset area ----
sf::st_read(paste0(wd, "/ctg/ctg.shp")) %>%
  sf::st_transform(epsg) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_centroid() %>%
  sf::st_buffer(2000) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf() -> subset_area

usethis::use_data(subset_area, overwrite = TRUE)





# Metrics infos ----
# lidar
lidar_metrics_desc <- c("dem" = "digital elevation model",
                        "dem_5m" = "digital elevation model",
                        "slope" = "slope in percent",
                        "aspect" = "slope orientation in degrees",
                        "z_above2" = "canopy cover of first returns above 2 m",
                        "z_above5" = "canopy cover of first returns above 5 m",
                        "z_above10" = "canopy cover of first returns above 10 m",
                        "z_above15" = "canopy cover of first returns above 15 m",
                        "z_mean" = "average height of returns above 1.3 m",
                        "z_cv" = "coefficient of variation of returns above 1.3 m",
                        "z_kurt" = "kurtosis  of returns above 1.3 m",
                        "z_max" = "maximum height of returns above 1.3 ms",
                        "z_p5" = "height of 5th percentiles of returns above 1.3 m",
                        "z_p10" = "height of 10th percentiles of returns above 1.3 m",
                        "z_p20" = "height of 20th percentiles of returns above 1.3 m",
                        "z_p30" = "height of 30th percentiles of returns above 1.3 m",
                        "z_p40" = "height of 40th percentiles of returns above 1.3 m",
                        "z_p50" = "height of 50th percentiles of returns above 1.3 m",
                        "z_p60" = "height of 60th percentiles of returns above 1.3 m",
                        "z_p70" = "height of 70th percentiles of returns above 1.3 m",
                        "z_p80" = "height of 80th percentiles of returns above 1.3 m",
                        "z_p90" = "height of 90th percentiles of returns above 1.3 m",
                        "z_p95" = "height of 95th percentiles of returns above 1.3 m",
                        "z_p99" = "height of 99th percentiles of returns above 1.3 m",
                        "rumple_index" = "rumple index of raw lidar data",
                        "z_qav" = "average square height of returns above 1.3 m",
                        "z_skew" = "skewness of returns above 1.3 m",
                        "z_sd" = "standard deviation of returns above 1.3 m",
                        "avg_p99" = "avg/p99",
                        "fractional_cover_05_2" = "number of returns between 0.5 m and 2 m divided by all returns below 2 m",
                        "fractional_cover_2_5" = "number of returns between 2 m and 5 m divided by all returns below 5 m",
                        "fractional_cover_5_10" = "number of returns between 5 m and 10 m divided by all returns below 10 m",
                        "relative_topographic_position_60m" = "relative topographic position with 3 pixels in x and y (60 m) from whiteboxtools",
                        "relative_topographic_position_100m" = "relative topographic position with 5 pixels in x and y (100 m) from whiteboxtools",
                        "relative_topographic_position_200m" = "relative topographic position with 10 pixels in x and y (200 m) from whiteboxtools",
                        "relative_topographic_position_400m" = "relative topographic position with 20 pixels in x and y (400 m) from whiteboxtools",
                        "relative_topographic_position_800m" = "relative topographic position with 40 pixels in x and y (800 m) from whiteboxtools",
                        "twi" = "topographic wetness index",
                        "twi_5m" = "topographic wetness index",
                        "sagawi" = "saga wetness index",
                        "sagawi_5m" = "saga wetness index")

tibble::tibble(path = list.files(paste0(wd, "/metrics/lidar"), pattern = "\\.tif$", full.names = TRUE)) %>%
  dplyr::mutate(name = list.files(paste0(wd, "/metrics/lidar"), pattern = "\\.tif$") %>% gsub(".tif", "", .),
                desc = lidar_metrics_desc[name],
                res = path %>%
                  purrr::map(terra::rast) %>%
                  purrr::map(terra::res) %>%
                  purrr::map(1) %>%
                  unlist(),
                type = "lidar") -> lidar_metrics_infos

# sentinel2
sentinel2_metrics_desc <- c("B2" = "Blue - 490 nm",
                            "B3" = "Green - 560 nm",
                            "B4" = "Red - 665 nm",
                            "B5" = "Vegetation Red Edge 1 (RE1) - 705 nm",
                            "B6" = "Vegetation Red Edge 2 (RE2) - 740 nm",
                            "B7sr" = "Surface Reflectance Vegetation Red Edge 3 - 783 nm",
                            "B8" = "Near Infrared (NIR) - 842 nm",
                            "B8sr" = "Surface Reflectance Near Infrared (NIRsr) - 842 nm",
                            "B8Asr" = "Surface Reflectance Narrow (Nsr) - 865 nm",
                            "B11" = "Shortwave Infrared 1 (SWIR1) - 1610 nm",
                            "B12" = "Shortwave Infrared 2 (SWIR2) - 2200 nm",
                            "NDVI" = "Normalized Difference Vegetation Index ((NIR - Red) / (NIR + Red))",
                            "GNDVI" = "Green Normalized Difference Vegetation Index ((NIR - Green) / (NIR + Green))",
                            "NDVIre" = "Normalized Difference Vegetation Index using Red Edge ((RE1 - Red) / (RE1 + Red))",
                            "RVI" = "Ratio Vegetation Index (NIR / Red)",
                            "DVI" = "Difference Vegetation Index (NIR - Red)",
                            "SAVI" = "Soil Adjusted Vegetation Index ((NIR - Red) / (NIR + Red + 0.5))*(1+0.5)",
                            "EVI" = "Enhanced Vegetation Index 2.5*((NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1))",
                            "GCI" = "Green Chlorophyll Index ((NIR / Green) - 1)",
                            "NDMI" = "Normalized Difference Moisture Index ((NIR - SWIR1) / (NIR + SWIR1))",
                            "NDWI" = "Normalized Difference Water Index ((NIR - SWIR2) / (NIR + SWIR2))")

tibble::tibble(path = list.files(paste0(wd, "/metrics/sentinel2"), pattern = "\\.tif$", full.names = TRUE)) %>%
  dplyr::mutate(name = list.files(paste0(wd, "/metrics/sentinel2"), pattern = "\\.tif$") %>% gsub(".tif", "", .),
                desc = sentinel2_metrics_desc[name],
                res = path %>%
                  purrr::map(terra::rast) %>%
                  purrr::map(terra::res) %>%
                  purrr::map(1) %>%
                  unlist(),
                type = "sentinel_2") -> sentinel2_metrics_infos

# dendro
dendro_metrics_desc <- c("AGB_ha" = "above ground biomass normalized per hectare (t C/ha)",
                         "AGB_ha_FOR" = "above ground biomass normalized per hectare (t C/ha) / mask for RMF",
                         "ba_ha" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m²/ha)",
                         "ba_ha_FOR" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m²/ha) / mask for RMF",
                         "ba_ha_min20" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m²/ha) / min 20 ??????", # À élucider le min 20
                         "dens" = "stem density with DBH > 7.1 cm (stems/ha)",
                         "dens_FOR" = "tem density with DBH > 7.1 cm (stems/ha) / mask for RMF",
                         "lor" = "lorey’s height /	average tree height weighted by basal area (m)",
                         "lor_FOR" = "lorey’s height /	average tree height weighted by basal area (m) / mask for RMF",
                         "qmdbh" = "quadratic mean of diameter at breast height (cm)",
                         "qmdbh_FOR" = "quadratic mean of diameter at breast height (cm) / mask for RMF",
                         "top_height" = "maximum height (m)",
                         "top_height_FOR" = "maximum height (m) / mask for RMF",
                         "V_ha" = "total whole stem volume per hectare	(m³/ha)",
                         "V_ha_FOR" = "total whole stem volume per hectare	(m³/ha) / mask for RMF",
                         "Vmerch_ha" = "total merchantable volume normalized per hectare where stump height is set to 0.2 m and minimum to diameter to 10 cm (m³/ha)",
                         "Vmerch_ha_FOR" = "total merchantable volume normalized per hectare where stump height is set to 0.2 m and minimum to diameter to 10 cm (m³/ha) / mask for RMF")

tibble::tibble(path = list.files(paste0(wd, "/metrics/dendro"), pattern = "\\.tif$", full.names = TRUE)) %>%
  dplyr::mutate(name = list.files(paste0(wd, "/metrics/dendro"), pattern = "\\.tif$") %>% gsub(".tif", "", .), # CA NE MARCHERA PAS POUR OVF, RENOMMER PLUS SIMPLEMENT
                desc = dendro_metrics_desc[name],
                res = path %>%
                  purrr::map(terra::rast) %>%
                  purrr::map(terra::res) %>%
                  purrr::map(1) %>%
                  unlist(),
                type = "dendrometric") %>%
  dplyr::filter(!grepl("_FOR$", name)) -> dendro_metrics_infos

bind_rows(lidar_metrics_infos,
          sentinel2_metrics_infos,
          dendro_metrics_infos) -> metrics_infos

rm(dendro_metrics_desc,
   lidar_metrics_desc,
   sentinel2_metrics_desc,
   lidar_metrics_infos,
   sentinel2_metrics_infos,
   dendro_metrics_infos)

usethis::use_data(metrics_infos, overwrite = TRUE)





# Metrics ----
metrics_infos %>%
  dplyr::filter(name %in% c(best_models_variables,
                            segmentation_metrics,
                            selected_dendrometrics,
                            "z_p95", "z_above2", "fractional_cover_05_2", "slope", "sagawi")) %T>%
  {dplyr::pull(.,name) ->> metrics_names} %>% # Extract metrics names in the right order
  dplyr::pull(path) %>%
  purrr::map(terra::rast) %>%
  purrr::map(terra::project, epsg, method = "bilinear") %>%
  purrr::map(terra::resample, .[[1]]) %>%
  terra::rast() %>%
  terra::crop(subset_area) -> metrics

names(metrics) <- metrics_names # Assign metrics names

metrics %>%
  terra::wrap() -> metrics

usethis::use_data(metrics, overwrite = TRUE)





# Masks ----
list(terra::vect(paste0(wd, "/shp/roads.shp")),
     terra::vect(paste0(wd, "/shp/waterbodies.shp"))) %>%
  purrr::map(terra::project, epsg) %>%
  purrr::map(terra::crop, subset_area) %>%
  purrr::map(terra::wrap) -> masks

usethis::use_data(masks, overwrite = TRUE)





# Ecoforest polygons ----
sf::st_read(paste0(wd, "/shp/PolygonForest.shp")) %>%
  tibble::rowid_to_column("id") %>%
  sf::st_transform(epsg) %>%
  sf::st_filter(subset_area) %>%
  rmapshaper::ms_clip(subset_area) -> fri_polygons

usethis::use_data(fri_polygons, overwrite = TRUE)





# Landcover ----
terra::rast(paste0(wd, "/metrics/other/landcover.tif")) %>%
  terra::crop(subset_area) -> landcover

landcover_codes <- c(`1` = "Clear_Open_Water",
                     `2` = "Turbid_Water",
                     `3` = "Shoreline",
                     `4` = "Mudflats",
                     `5` = "Marsh",
                     `6` = "Swamp",
                     `7` = "Fen",
                     `8` = "Bog",
                     `10` = "Heath",
                     `11` = "Sparse_Treed",
                     `12` = "Treed_Upland",
                     `13` = "Deciduous_Treed",
                     `14` = "Mixed_Treed",
                     `15` = "Coniferous_Treed",
                     `16` = "Plantations_Treed_Cultivated",
                     `17` = "Hedge_Rows",
                     `18` = "Disturbance",
                     `19` = "Open_Cliff_Talus",
                     `20` = "Alvar",
                     `21` = "Sand_Barren_Dune",
                     `22` = "Open_Tallgrass_Prairie",
                     `23` = "Tallgrass_Savannah",
                     `24` = "Tallgrass_Woodland",
                     `25` = "Sand_Gravel_Mine_Tailings_Extraction",
                     `26` = "Bedrock",
                     `27` = "Communit_Infrastructure",
                     `28` = "Agriculture_Undifferentiated_Rural_Land_Use",
                     `157` = "Other",
                     `247` = "Cloud_Shadow")

# Attach levels (lookup table)
levels(landcover) <- data.frame(value = as.integer(names(landcover_codes)),
                                class = unname(landcover_codes))

landcover %>%
  terra::wrap() -> landcover

usethis::use_data(landcover, overwrite = TRUE)





# Forest age in 2019 ----
terra::rast(paste0(wd, "/metrics/other/forest_age_2019.tif")) %>%
  terra::crop(subset_area) -> forest_age_2019

forest_age_2019 %>%
  terra::wrap() -> forest_age_2019

usethis::use_data(forest_age_2019, overwrite = TRUE)



# Forest fire between 1985-2020 ----
terra::rast(paste0(wd, "/metrics/other/forest_fire_1985_2020.tif")) %>%
  terra::crop(subset_area) -> forest_fire_1985_2020

forest_fire_1985_2020 %>%
  terra::wrap() -> forest_fire_1985_2020

usethis::use_data(forest_fire_1985_2020, overwrite = TRUE)



# Forest harvest between 1985-2020 ----
terra::rast(paste0(wd, "/metrics/other/forest_harvest_1985_2020.tif")) %>%
  terra::crop(subset_area) -> forest_harvest_1985_2020

forest_harvest_1985_2020 %>%
  terra::wrap() -> forest_harvest_1985_2020

usethis::use_data(forest_harvest_1985_2020, overwrite = TRUE)
