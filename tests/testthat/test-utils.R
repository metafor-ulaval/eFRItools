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
