#' Create a dataframe with all available metrics informations
#'
#' @param metric_path list of folder where metrics are with metric type as folder name
#'
#' @returns
#' A dataframe with all available metrics informations
#' @export
#'
#' @examples
#' read_metrics(c("D:/00_Ontario_eFRI/RMF/metrics/lidar",
#'                "D:/00_Ontario_eFRI/RMF/metrics/sentinel2",
#'                "D:/00_Ontario_eFRI/RMF/metrics/dendro")) -> metrics_infos
read_metrics <- function(metric_path){

  description <- c("dem" = "digital elevation model",
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
                   "sagawi_5m" = "saga wetness index",
                   "B2" = "Blue - 490 nm",
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
                   "NDWI" = "Normalized Difference Water Index ((NIR - SWIR2) / (NIR + SWIR2))",
                   "AGB_ha" = "above ground biomass normalized per hectare (t C/ha)",
                   "AGB_ha_FOR" = "above ground biomass normalized per hectare (t C/ha) / mask for RMF",
                   "ba_ha" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m2/ha)",
                   "ba_ha_FOR" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m2/ha) / mask for RMF",
                   "ba_ha_min20" = "basal area / tree cross sectional area (approximated as a circle) at breast height (1.3 m) (m2/ha) / min 20 ??????", # À élucider le min 20
                   "dens" = "stem density with DBH > 7.1 cm (stems/ha)",
                   "dens_FOR" = "tem density with DBH > 7.1 cm (stems/ha) / mask for RMF",
                   "lor" = "lorey’s height /	average tree height weighted by basal area (m)",
                   "lor_FOR" = "lorey’s height /	average tree height weighted by basal area (m) / mask for RMF",
                   "qmdbh" = "quadratic mean of diameter at breast height (cm)",
                   "qmdbh_FOR" = "quadratic mean of diameter at breast height (cm) / mask for RMF",
                   "top_height" = "maximum height (m)",
                   "top_height_FOR" = "maximum height (m) / mask for RMF",
                   "V_ha" = "total whole stem volume per hectare	(m3/ha)",
                   "V_ha_FOR" = "total whole stem volume per hectare	(m3/ha) / mask for RMF",
                   "Vmerch_ha" = "total merchantable volume normalized per hectare where stump height is set to 0.2 m and minimum to diameter to 10 cm (m3/ha)",
                   "Vmerch_ha_FOR" = "total merchantable volume normalized per hectare where stump height is set to 0.2 m and minimum to diameter to 10 cm (m3/ha) / mask for RMF")

  data_info <- lapply(metric_path,
                      function(x){
                        path <- list.files(x, pattern = "\\.tif$", full.names = TRUE)
                        name <- gsub("\\.tif$", "", basename(path))
                        resolution <- sapply(path, function(p) {terra::res(terra::rast(p))[1]})
                        type <- basename(x)

                        data.frame(path = path,
                                   name = name,
                                   resolution = resolution,
                                   type = type,
                                   row.names = NULL,
                                   stringsAsFactors = FALSE)
                      })

  data_info <- do.call(rbind, data_info)

  data_info$description <- description[data_info$name]

  return(data_info)
}


