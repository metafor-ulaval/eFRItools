test_that("centroid ok", {
  fri_polygons_geometry <- st_geometry(fri_polygons)

  fri_polygons_centroids <- mutate_centroid(fri_polygons_geometry)

  expect_equal(fri_polygons_centroids[1,]$X, 447635.72)

  expect_equal(fri_polygons_centroids[1,]$Y, 5348763.3)
})
