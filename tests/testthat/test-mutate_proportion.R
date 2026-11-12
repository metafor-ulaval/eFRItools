test_that("prop correctly extracted", {
  fri_polygons_landcover_prop <- mutate_proportion(fri_polygons, terra::rast(landcover), "landcover")

  expect_equal(fri_polygons_landcover_prop$MOST_FREQUENT_LANDCOVER[1], "MIXED_TREED")
})

test_that("a tie for the most frequent class does not depend on the other polygons", {
  # Two cells: class 5 on the left, class 3 on the right
  r <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1, crs = "EPSG:32617", vals = c(5, 3))
  names(r) <- "class"
  square <- function(xmin, xmax) sf::st_polygon(list(rbind(c(xmin, 0), c(xmax, 0), c(xmax, 1), c(xmin, 1), c(xmin, 0))))
  tie <- square(0, 2) # Half class 5, half class 3
  only_5 <- square(0, 1)

  alone <- mutate_proportion(sf::st_sf(geometry = sf::st_sfc(tie, crs = 32617)), r, "test")
  after_5 <- mutate_proportion(sf::st_sf(geometry = sf::st_sfc(only_5, tie, crs = 32617)), r, "test")

  expect_equal(alone$MOST_FREQUENT_TEST, "3")
  expect_equal(after_5$MOST_FREQUENT_TEST[2], "3")
  expect_equal(after_5$MOST_FREQUENT_TEST_PROPORTION[2], 50)
})
