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

test_that("species found", {
  expect_equal(assign_common_name("SB"), "black spruce")
})

test_that("species dont exist", {
  expect_equal(assign_common_name("SBB"), NA_character_)
})

test_that("coniferous found", {
  expect_equal(assign_type("pine"), "Coniferous")
})

test_that("deciduous found", {
  expect_equal(assign_type("maple"), "Deciduous")
})

test_that("sum is correct", {
  raster <- terra::rast(landcover)
  subset_area %>%
    sf::st_transform(sf::st_crs(raster)) %>%
    exactextractr::exact_extract(raster, ., coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover) %>%
    dplyr::bind_rows() -> raster_proportion

  expect_equal(raster_proportion$class_area[1], 301599.6)
})
