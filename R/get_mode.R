#' Title
#'
#' @param x A vector
#'
#' @returns The mode
#' @export
#'
#' @examples
#'   x <- c("a", "b", "c", "a")
#'   get_mode(x)
get_mode <- function(x){
  unique_val <- unique(x)
  mode_val <- unique_val[which.max(tabulate(match(x, unique_val)))]
  return(mode_val)
}
