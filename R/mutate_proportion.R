#' Extract the proportion of each class of a raster in specified polygons
#'
#' @param x sf; polygons from the `sf` package.
#' @param y SpatRaster; categorical raster from the `terra` package.
#' @param prefix character; prefix of the created columns.
#' @param remove_class character; classes to remove, like "NA" or "0".
#' @param simplify logical; compute the most frequent class and its proportion. When several classes have the same proportion, the lowest class value is kept.
#' @param keep_all logical; keep the proportion columns of each class when `simplify = TRUE`.
#'
#' @returns
#' Polygons with extracted class proportions as individual columns
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
  x_sum_cover <- exactextractr::exact_extract(y, x_crs, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover, progress = FALSE)

  # Proportion of each class in each polygon (one row per polygon, one column per class).
  # Areas are computed once for all polygons, and the table is filled in one step.
  # A class absent from a polygon is NA.
  area <- as.numeric(sf::st_area(x_crs))
  cover <- dplyr::bind_rows(x_sum_cover, .id = "polygon")
  cover <- cover[!is.na(cover$value), , drop = FALSE]
  polygon <- as.integer(cover$polygon)
  cover_class <- as.character(cover$value)
  classes <- unique(cover_class)

  x_proportion <- matrix(NA_real_, nrow = length(x_sum_cover), ncol = length(classes), dimnames = list(NULL, classes))
  x_proportion[cbind(polygon, match(cover_class, classes))] <- cover$coverage_area / area[polygon] * 100
  x_proportion <- as.data.frame(x_proportion)

  # NA pixels are counted in the NA column with the area not covered by the raster
  covered <- vapply(x_sum_cover, function(xx) sum(xx$coverage_area[!is.na(xx$value)]), numeric(1))
  x_proportion$`NA` <- (area - covered) / area * 100

  x_proportion <- x_proportion[,!(names(x_proportion) %in% remove_class), drop = FALSE]

  # Columns are sorted by class value: bind_rows() orders them by first appearance across
  # the polygons, so a tie for the most frequent class would depend on the other polygons.
  # Sorted, a tie always goes to the lowest class value.
  class_value <- suppressWarnings(as.numeric(names(x_proportion)))
  x_proportion <- x_proportion[, order(class_value, names(x_proportion)), drop = FALSE]

  y_class <- terra::cats(y)[[1]]

  if(!is.null(y_class)){

    y_class[nrow(y_class) + 1,] <- "NA"
    names(x_proportion) <- y_class$class[match(names(x_proportion), y_class$value)]

  }

  if(ncol(x_proportion) != 0){

    x_proportion_results <- x_proportion
    names(x_proportion_results) <- paste0(prefix, "_", names(x_proportion_results), "_proportion")

    if(simplify == TRUE){

      # Most frequent class of each polygon (NA when the polygon has no class)
      x_proportion_matrix <- as.matrix(x_proportion)
      column <- which_max_row(x_proportion_matrix)
      x_proportion_simplified <- data.frame(name = toupper(names(x_proportion)[column]),
                                            value = x_proportion_matrix[cbind(seq_along(column), column)])
      names(x_proportion_simplified) <- c(paste0("most_frequent_", prefix), paste0("most_frequent_", prefix, "_proportion"))

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
