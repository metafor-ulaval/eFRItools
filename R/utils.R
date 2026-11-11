get_mode <- function(x) {

  unique_val <- unique(x)
  unique_val[which.max(tabulate(match(x, unique_val)))]

}

# Median of the cell values weighted by their coverage fraction.
# exactextractr merges equal values before interpolating its quantiles, which
# biases the median when many cells share the same value (e.g. 0, 0, 0, 10 gives 5).
# Here each cell keeps its own weight: the median is the first value reaching half
# of the total weight, or the mean of the two middle values when a cell ends exactly
# at half (same result as stats::median() when all weights are equal).
weighted_median <- function(values, coverage_fraction){

  keep <- !is.na(values) & coverage_fraction > 0
  values <- values[keep]
  coverage_fraction <- coverage_fraction[keep]

  if (length(values) == 0) {return(NA_real_)}

  ord <- order(values)
  values <- values[ord]
  cum_weight <- cumsum(coverage_fraction[ord]) / sum(coverage_fraction)

  i <- which(cum_weight >= 0.5 - 1e-9)[1]
  if (abs(cum_weight[i] - 0.5) < 1e-9 && i < length(values)) {
    (values[i] + values[i + 1]) / 2
  } else {
    values[i]
  }
}

# Summarize the cells of y in each polygon of x.
# "median" uses weighted_median(); any other exactextractr summary is passed as is.
extract_summary <- function(y, x, fun){

  if (identical(fun, "median")) {
    exactextractr::exact_extract(y, x, fun = weighted_median, stack_apply = TRUE, progress = FALSE)
  } else {
    exactextractr::exact_extract(y, x, fun = fun, progress = FALSE)
  }
}

sum_cover <- function(x){

  x <- x[!is.na(x$value), ]

  if (nrow(x) == 0) {return(list(data.frame(value = NA_real_, coverage_area = 0)))}

  list(stats::aggregate(coverage_area ~ value, data = x, FUN = sum))
}

imputation <- function(reference_polygons,
                       target_variable,
                       nn){

  # Extract values of the variable for the reference data
  data <- reference_polygons[[target_variable]]

  # The apply function allows, for each row of the nn matrix which is a target polygon, to extract the observations (values in nn matrix are index) of the reference polygons
  if (is.numeric(data))
    # For numeric values, the mean of the k observations from reference data is used to impute value into the target polygon
    imputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ mean(data[x]) })
  else if (is.factor(data) || is.character(data))
    # For categorical values, the mode of the k observations from reference data is used to impute value into the target polygon
    imputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ get_mode(data[x]) })
  else
    stop(paste("Non supported column type", target_variable))

  return(imputation_result)
}
