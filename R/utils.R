get_mode <- function(x) {
  unique_val <- unique(x)
  unique_val[which.max(tabulate(match(x, unique_val)))]
}
