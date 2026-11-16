test_that("pve is computed on the cells covered by the polygons only", {
  # 4 x 4 grid: the two left columns are covered by polygon 1, the third by polygon 2,
  # the fourth by no polygon. One cell of polygon 1 is NA.
  vals <- c(1, 2, 10, 100,
            3, NA, 12, 100,
            1, 2, 11, 100,
            3, 4, 13, 100)
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:32617", vals = vals)
  square <- function(xmin, xmax) sf::st_polygon(list(rbind(c(xmin, 0), c(xmax, 0), c(xmax, 4), c(xmin, 4), c(xmin, 0))))
  x <- sf::st_sf(geometry = sf::st_sfc(square(0, 2), square(2, 3), crs = 32617))

  value <- c(1, 2, 3, 1, 2, 3, 4, 10, 12, 11, 13)
  group <- c(rep(1, 7), rep(2, 4))
  expected <- 1 - sum((value - ave(value, group))^2) / sum((value - mean(value))^2)

  expect_equal(proportion_variance_explained(x, r), expected)
})

test_that("a polygon that covers no cell centre is ignored", {
  r <- terra::rast(nrows = 1, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 1, crs = "EPSG:32617", vals = c(1, 3, 10, 14))
  square <- function(xmin, xmax) sf::st_polygon(list(rbind(c(xmin, 0), c(xmax, 0), c(xmax, 1), c(xmin, 1), c(xmin, 0))))
  x <- sf::st_sf(geometry = sf::st_sfc(square(0, 2), square(3.6, 3.9), square(2, 3.4), crs = 32617))

  value <- c(1, 3, 10)
  group <- c(1, 1, 3)
  expected <- 1 - sum((value - ave(value, group))^2) / sum((value - mean(value))^2)

  expect_equal(proportion_variance_explained(x, r), expected)
})
