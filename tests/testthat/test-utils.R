test_that("get mode works", {
  x <- c("a", "b", "c", "a")
  expect_equal(get_mode(x), "a")
})

test_that("get mode works", {
  x <- c("a", "b", "c", "a", "b")
  expect_equal(get_mode(x), "a")
})

test_that("get mode works", {
  x <- c(1, 2, 3, 1)
  expect_equal(get_mode(x), 1)
})

test_that("sum is correct", {
  raster <- terra::rast(landcover)
  area <- sf::st_transform(subset_area, sf::st_crs(raster))
  raster_proportion <- exactextractr::exact_extract(raster, area, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover)
  raster_proportion <- dplyr::bind_rows(raster_proportion)

  expect_equal(raster_proportion$coverage_area[1], 301599.6)
})

test_that("st_join_largest gives the same result as st_join(largest = TRUE)", {
  square <- function(xmin, xmax, ymin = 0, ymax = 10) sf::st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin))))
  # Tiles: A [0, 10] 2017, B [10, 20] 2018, C [20, 30] 2018
  tiles <- sf::st_sf(year = c(2017, 2018, 2018), geometry = sf::st_sfc(square(0, 10), square(10, 20), square(20, 30), crs = 32617))
  polygons <- sf::st_sf(id = 1:4,
                        geometry = sf::st_sfc(square(2, 8),    # Inside A
                                              square(7, 17),   # 30 % in A, 70 % in B
                                              square(15, 25),  # B and C, same year
                                              square(40, 50),  # No tile
                                              crs = 32617))

  joined <- st_join_largest(polygons, tiles, "year")

  expect_equal(joined$year, c(2017, 2018, 2018, NA))
  expect_identical(joined, suppressWarnings(sf::st_join(polygons, tiles["year"], largest = TRUE)))
})

test_that("weighted median handles repeated values", {
  expect_equal(weighted_median(c(0, 0, 0, 10), rep(1, 4)), 0)
  expect_equal(weighted_median(c(14, 14, 14, 60, 90), rep(1, 5)), 14)
})

test_that("weighted median equals stats::median with equal weights", {
  set.seed(1)
  for (n in c(1, 2, 7, 10)) {
    x <- round(runif(n, 0, 100))
    expect_equal(weighted_median(x, rep(1, n)), stats::median(x))
  }
})

test_that("weighted median uses the coverage fraction and ignores NA", {
  expect_equal(weighted_median(c(1, 5, 9), c(0.1, 0.2, 1)), 9)
  expect_equal(weighted_median(c(1, NA, 9, 9), c(1, 1, 0.5, 0.5)), 5)
  expect_true(is.na(weighted_median(c(NA, NA), c(1, 1))))
})

test_that("extract_summary median is not biased by repeated cell values", {
  raster <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:32617", vals = c(0, 0, 0, 10))
  polygon <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 2, ymax = 2), crs = 32617)))

  expect_equal(extract_summary(raster, polygon, "median"), 0)
  expect_equal(extract_summary(raster, polygon, "mean"), 2.5)
})
