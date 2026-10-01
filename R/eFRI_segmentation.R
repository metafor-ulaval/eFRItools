#' Segmentation
#'
#' @param metrics SpatRaster; multiband raster to segment from the `terra` package.
#' @param output_path character; folder where the segmentation raster is saved.
#' @param output_name character; file name of the segmentation raster, without extension.
#' @inheritParams pre_processing
#' @inheritParams generic_region_merging
#'
#' @returns
#' Polygons of the segmentation from the `sf` package
#' @export
#'
#' @examples
#' print("example")
eFRI_segmentation <- function(metrics,
                              masks = NULL,
                              thresh = 47,
                              spec = 0.6,
                              spat = 0.6,
                              method = "bs",
                              clean_nodata = TRUE,
                              output_path,
                              output_name,
                              otb_dir){

    metrics_preprocessed <- pre_processing(metrics, masks)

    cat("Segmentation\n")
    segmentation <- generic_region_merging(metrics_preprocessed,
                                           ofile = paste0(output_path, "/", output_name, ".tif"),
                                           thresh = thresh,
                                           spec = spec,
                                           spat = spat,
                                           method = method,
                                           clean_nodata = clean_nodata,
                                           otb_dir = otb_dir)

    segmentation <- sf::st_cast(segmentation, "MULTIPOLYGON")
    segmentation <- sf::st_cast(segmentation, "POLYGON")
    colnames(segmentation)[1] <- "id_seg"
    segmentation$id <- seq_len(nrow(segmentation))

    return(segmentation)

}
