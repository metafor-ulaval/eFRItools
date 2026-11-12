#' eFRI_imputation
#'
#' Imputes forest attributes into the segmentation polygons with a k-nearest neighbour approach,
#' using the reference polygons built by [eFRI_build_imputation_reference()].
#'
#' @param segmentation sf; polygons from the `sf` package produced by [eFRI_segmentation()] or [eFRI_attribute_table()].
#' @param reference sf; reference polygons built by [eFRI_build_imputation_reference()].
#' @param metrics SpatRaster; metrics with one layer per metric from the `terra` package. Must contain the metrics used in `knn_variables`.
#' @param target_variables character; names of the columns of `reference` to impute.
#' @param knn_variables character; comma-separated metric names used for the knn of each `target_variables`, of the same length as `target_variables`. "X" and "Y" are the polygon centroid.
#'
#' @returns
#' `segmentation` with imputed variables as individual columns
#' @export
#'
#' @examples
#' eFRI_imputation
eFRI_imputation <- function(segmentation,
                            reference,
                            metrics,
                            target_variables,
                            knn_variables){

  if(nrow(reference) == 0){
    cat("No polygon available for imputation\n")
    return(segmentation)
  }

  # knn variables, as named in the reference and segmentation columns
  knn_columns <- lapply(strsplit(knn_variables, ","), function(v){
    c(paste0(v[v != "X" & v != "Y"], "_median"), v[v == "X" | v == "Y"])
  })

  missing <- setdiff(c(unlist(knn_columns), target_variables), names(reference))
  if(length(missing) > 0){
    stop("Columns missing in reference: ", paste(missing, collapse = ", "),
         ". Build the reference with eFRI_build_imputation_reference() using metrics that contain the knn variables.")
  }

  # Extract data for imputation into segmentation
  cat("Extract data of segmentation\n")
  segmentation$id <- 1:nrow(segmentation)
  segmentation_data <- segmentation
  segmentation_data <- mutate_centroid(segmentation_data)
  segmentation_data <- mutate_metrics(segmentation_data, metrics, fun = "median")

  # Perform imputation
  for(i in seq_along(target_variables)){

    cat(paste0("Imputation of variable : ", target_variables[i], "\n"))

    knn_variables_temp <- knn_columns[[i]]

    reference_temp <- reference[c(knn_variables_temp, target_variables[i])]
    reference_temp <- stats::na.omit(reference_temp)

    segmentation_temp <- segmentation_data[c("id", knn_variables_temp)]
    segmentation_temp <- stats::na.omit(segmentation_temp)

    segmentation_temp[target_variables[i]] <- knn_imputation(reference_polygons = reference_temp,
                                                             target_polygons = segmentation_temp,
                                                             knn_variables = knn_variables_temp,
                                                             target_variables = target_variables[i],
                                                             k = 5)
    segmentation <- merge(segmentation,
                          sf::st_drop_geometry(segmentation_temp[c("id", target_variables[i])]),
                          by = "id",
                          all = TRUE)
  }

  return(segmentation)

}
