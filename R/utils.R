get_mode <- function(x) {

  unique_val <- unique(x)
  unique_val[which.max(tabulate(match(x, unique_val)))]

}

sum_cover <- function(x){

  list(x |>
         dplyr::group_by(value) |>
         dplyr::summarise(class_area = sum(coverage_area)))

}

imputation <- function(reference_polygons,
                       var,
                       nn){

  # Extract values of the variable for the reference data
  data <- reference_polygons[[var]]

  # The apply function allows, for each row of the nn matrix which is a target polygon, to extract the observations (values in nn matrix are index) of the reference polygons
  if (is.numeric(data))
    # For numeric values, the mean of the k observations from reference data is used to impute value into the target polygon
    inputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ mean(data[x]) })
  else if (is.factor(data) || is.character(data))
    # For categorical values, the mode of the k observations from reference data is used to impute value into the target polygon
    inputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ get.mode(data[x]) })
  else
    stop(paste("Non supported column type", var))

  return(inputation_result)
}
