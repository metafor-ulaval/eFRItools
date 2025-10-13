test_that("age is correct", {
  fri_polygons_age <- mutate_age(fri_polygons, terra::rast(forest_age_2019), 2019, 2025, "mean")

  expect_equal(fri_polygons_age[1,]$age_mean_2025, 86)
})
