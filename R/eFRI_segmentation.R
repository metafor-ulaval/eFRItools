#' Segmentation
#'
#' @param metrics param
#' @param masks param
#' @param thresh param
#' @param spec param
#' @param spat param
#' @param method param
#' @param clean_nodata param
#' @param output_path param
#' @param output_name param
#' @param otb_dir param
#'
#' @returns
#' segmentation
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

    cat("Segmentation/n")
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
