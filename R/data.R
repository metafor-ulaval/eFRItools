#' Subset area for testing
#'
#' Subset area in a 2 km radius circle in the centroid of area for different testing purposes
#'
#' @format Subset area in a 2 km radius circle in the centroid of area in [sf][sf::sf]
#' @source Internal
"subset_area"

#' Information about all metrics
#'
#' A tibble that contains all informations about metrics that can be used : path to data, metric name, description, resolution and type (lidar, sentinel-2 and dendrometric)
#'
#' @format A tibble
#' @source Internal
"metrics_infos"

#' A subset of metrics used for examples
#'
#' A subset of metrics used for examples : dem, fractional_cover_05_2, relative_topographic_position_100m, rumple_index, sagawi, slope, z_above2, z_cv, z_kurt, z_p95, B6, NDVI, ba_ha, dens, lor, qmdbh, Vmerch_ha
#'
#' @format A SpatRaster [rast][terra::rast]
#' @source Internal
"metrics"

#' Masks
#'
#' Masks used to perform segmentation : road ([[1]]) and waterbodies ([[2]])
#'
#' @format A SpatVector [vect][terra::vect]
#' @source Internal
"masks"

#' FRI polygons
#'
#' Forest Resources Inventory polygons
#'
#' @format Forest Resources Inventory polygons in [sf][sf::sf]
#' @source Internal
"fri_polygons"

#' Landcover
#'
#' Landcover from the Ontario Land Cover Compilation v.2.0
#'
#' @format A SpatRaster of landcover from the Ontario Land Cover Compilation v.2.0 [rast][terra::rast]
#' @source Internal
"landcover"

#' Forest age
#'
#' Landsat-derived forest age for Canada 2019 [https://opendata.nfis.org/downloads/forest_change/CA_forest_age_2019.zip]
#'
#' @format A SpatRaster of Landsat-derived forest age for Canada 2019 [rast][terra::rast]
#' @source Internal
"forest_age_2019"

#' Forest fire
#'
#' Landsat-derived forest wildfire disturbances for Canada 1985-2020. [https://opendata.nfis.org/downloads/forest_change/CA_Forest_Fire_1985-2020.zip]
#'
#' @format A SpatRaster of Landsat-derived forest wildfire disturbances for Canada 1985-2020 [rast][terra::rast]
#' @source Internal
"forest_fire_1985_2020"

#' Forest harvest
#'
#' Landsat-derived forest harvest disturbances for Canada 1985-2020 [https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip]
#'
#' @format A SpatRaster of Landsat-derived forest harvest disturbances for Canada 1985-2020 [rast][terra::rast]
#' @source Internal
"forest_harvest_1985_2020"
