test_that("age is correct", {
  fri_polygons_age <- mutate_age(fri_polygons, terra::rast(forest_age_2019), 2019, 2025, "median")

  expect_equal(fri_polygons_age[1,]$age_median_2025, 83)
})

test_that("disturbed pixels take the time since disturbance", {
  forest_age <- terra::rast(forest_age_2019)
  disturbance <- terra::init(forest_age, 2015)
  fri_polygons_age <- mutate_age(fri_polygons[1:5,], forest_age, 2019, 2025, "median", disturbance = disturbance)

  expect_equal(fri_polygons_age$age_median_2025, rep(10, 5))

  no_disturbance <- terra::init(forest_age, 0)
  expect_equal(mutate_age(fri_polygons, forest_age, 2019, 2025, "median", disturbance = no_disturbance)$age_median_2025,
               mutate_age(fri_polygons, forest_age, 2019, 2025, "median")$age_median_2025)
})
