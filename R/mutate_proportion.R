#' Extract the proportion of each class of a raster in specified polygons
#'
#' @param x spatial features; polygons of Forest Resources Inventory from the `sf` package.
#' @param y raster; SpatRaster with categorical values from the `terra` package.
#' @param prefix character; prefix use for created column for each class
#' @param remove_class character; class needed to be removed, like na or 0
#' @param simplify logical; compute the most frequent class and its proportion
#' @param keep_all logical; keep all column of individual class
#'
#' @returns
#' Polygons with extracted class proportions as individuals columns
#' @export
#'
#' @examples
#' library(sf)
#' library(terra)
#'
#' fri_polygons_landcover_prop <- mutate_proportion(fri_polygons, rast(landcover), "landcover")
#'
#' fri_polygons_landcover_prop
mutate_proportion <- function(x,
                              y,
                              prefix,
                              remove_class = c("NaN" ,"NA", "0"),
                              simplify = TRUE,
                              keep_all = TRUE){

  cat(paste0("Mutate proportion for ", nrow(x), " polygon(s) of raster ", names(y), "\n"))
  x_crs <- sf::st_transform(x, sf::st_crs(y))
  x_sum_cover <- exactextractr::exact_extract(y, x_crs, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover)

  x_proportion <- lapply(seq_along(x_sum_cover),
                         function(x){
                           area <- sf::st_area(x_crs[x,])
                           area <- as.numeric(area)
                           coverage_proportion <- t(x_sum_cover[[x]]$coverage_area / area * 100)
                           coverage_proportion <- as.data.frame(coverage_proportion)
                           colnames(coverage_proportion) <- x_sum_cover[[x]]$value
                           coverage_proportion$`NA` <- (area-sum(x_sum_cover[[x]]$coverage_area)) / area * 100
                           return(coverage_proportion)
                         })

  x_proportion <- dplyr::bind_rows(x_proportion)
  x_proportion <- x_proportion[,!(names(x_proportion) %in% remove_class), drop = FALSE]

  y_class <- terra::cats(y)[[1]]

  if(!is.null(y_class)){

    y_class[nrow(y_class) + 1,] <- "NA"
    names(x_proportion) <- y_class$class[match(names(x_proportion), y_class$value)]

  }

  if(ncol(x_proportion) != 0){

    x_proportion_results <- x_proportion
    names(x_proportion_results) <- paste0(prefix, "_", names(x_proportion_results), "_proportion")

    if(simplify == TRUE){

      x_proportion_simplified <- lapply(seq_len(nrow(x_proportion)),
                                        function(xx){

                                          x_proportion_temp <- x_proportion[xx, , drop = FALSE]
                                          x_simplified <- x_proportion_temp[which.max(x_proportion_temp)]

                                          if(all(is.na(x_proportion_temp))){
                                            x_simplified <- data.frame(name = NA,
                                                                       value = NA)
                                          } else {
                                            x_simplified <- data.frame(name = toupper(names(x_simplified)),
                                                                       value = as.numeric(x_simplified))
                                          }

                                          names(x_simplified) <- c(paste0("most_frequent_", prefix), paste0("most_frequent_", prefix, "_proportion"))

                                          return(x_simplified)
                                        })

      x_proportion_simplified <- do.call(rbind, x_proportion_simplified)

      x_proportion_results <- dplyr::bind_cols(x_proportion_results, x_proportion_simplified)

      if(keep_all == FALSE){

        x_proportion_results <- x_proportion_results[,names(x_proportion_simplified)]

      }

    }

  } else {

    x_proportion_results <- x_proportion

  }

  names(x_proportion_results) <- toupper(names(x_proportion_results))

  x[names(x_proportion_results)] <- x_proportion_results[names(x_proportion_results)]

  return(x)

}
