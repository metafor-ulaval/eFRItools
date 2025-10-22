test_that("prop correctly extracted", {
  fri_polygons_landcover_prop <- mutate_proportion(fri_polygons, terra::rast(landcover), "landcover")

  expect_equal(fri_polygons_landcover_prop$MOST_FREQUENT_LANDCOVER[1], "MIXED_TREED")
})
