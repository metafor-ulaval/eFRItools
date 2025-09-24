get_mode <- function(x){
  unique_val <- unique(x)
  mode_val <- unique_val[which.max(tabulate(match(x, unique_val)))]
  return(mode_val)
}
