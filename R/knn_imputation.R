#' Perform knn imputation
#'
#' @param reference_polygons sf; reference polygons from the `sf` package with known values of `target_variables`.
#' @param target_polygons sf; polygons from the `sf` package to impute. If identical to `reference_polygons`, each polygon is excluded from its own neighbours.
#' @param knn_variables character; names of the columns used to find the nearest neighbours.
#' @param target_variables character; names of the columns of `reference_polygons` to impute.
#' @param k numeric; number of nearest neighbours.
#'
#' @returns
#' Data frame of the imputed variables with the same length as `target_polygons`
#' @export
#'
#' @examples
#' "ex"
knn_imputation <- function(reference_polygons,
                           target_polygons,
                           knn_variables,
                           target_variables,
                           k = 5){

  # If the reference is the same as the target, the polygon itself must not be used as a neighbour
  self_search <- identical(reference_polygons, target_polygons)

  if (self_search)
  {
    cat("Self search mode\n")
    k = k+1
  }

  # Extract knn variable used for imputation
  target <- sf::st_drop_geometry(target_polygons)
  target <- target[, knn_variables, drop = FALSE]

  reference <- sf::st_drop_geometry(reference_polygons)
  reference <- reference[, knn_variables, drop = FALSE]

  # Combine and scale data
  dat_comb_scaled <- scale(rbind(target, reference))

  # Re-extract data for each dataset into a matrix
  reference <- dat_comb_scaled[(nrow(target)+1):(nrow(target)+nrow(reference)), , drop = FALSE]
  target <- dat_comb_scaled[1:nrow(target), , drop = FALSE]

  # Run knn algorithm
  nn <- RANN::nn2(reference, target, k = k)

  # Extract index
  nn <- nn[[1]]

  # If the reference is the same as the target, the first nearest neighbours is removed
  if (self_search)
  {
    nn <- nn[, 2:k, drop = FALSE]
  }

  knn_imputation_result <- lapply(target_variables, imputation, nn = nn, reference_polygons = reference_polygons)
  knn_imputation_result <- as.data.frame(knn_imputation_result)
  names(knn_imputation_result) <- target_variables

  return(knn_imputation_result)
}
