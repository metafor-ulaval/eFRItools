knn_inputation <- function(reference_polygons,
                           target_polygons,
                           knn_variables,
                           target_variables,
                           k = 5){

  # Extract adress (unique value) of the object to see if the reference is the same as the target
  addr1 <- data.table::address(reference_polygons)
  addr2 <- data.table::address(target_polygons)

  if (addr1 == addr2)
  {
    print("Self search mode")
    k = k+1
  }

  # Extract knn variable used for imputation
  target_polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::select(all_of(knn_variables)) -> target

  reference_polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::select(all_of(knn_variables)) -> reference

  # Combine and scale data
  dplyr::bind_rows(target,
                   reference) %>%
    scale() -> dat_comb_scaled

  # Re-extract data for each dataset into a matrix
  reference <- dat_comb_scaled[(nrow(target)+1):(nrow(target)+nrow(reference)),]
  target <- dat_comb_scaled[1:nrow(target),]

  # Run knn algorithm
  nn <- RANN::nn2(reference, target, k = k)

  # Extract index
  nn <- nn[[1]]

  # If the reference is the same as the target, the first nearest neighbours is removed
  if (addr1 == addr2)
  {
    nn <- nn[, 2:k]
  }

  knn_inputation_result <- lapply(target_variables, imputation, nn = nn, reference_polygons = reference_polygons)
  knn_inputation_result <- as.data.frame(knn_inputation_result)
  names(knn_inputation_result) <- target_variables

  return(knn_inputation_result)
}
