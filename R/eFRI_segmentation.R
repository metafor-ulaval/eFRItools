#' Segmentation
#'
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
                              ofile,
                              thresh = 50,
                              spec = 0.5,
                              spat = 0.5,
                              method = "bs",
                              clean_nodata = TRUE,
                              otb_dir){

    metrics_preprocessed <- pre_processing(metrics, masks)

    cat("Segmentation\n")
    segmentation <- generic_region_merging(metrics_preprocessed,
                                           ofile = ofile,
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
