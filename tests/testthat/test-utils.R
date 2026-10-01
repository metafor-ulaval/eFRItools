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
