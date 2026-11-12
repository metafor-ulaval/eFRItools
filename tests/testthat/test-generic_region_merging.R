test_that("small polygons are merged or removed, and no multipart polygon is left", {
  # Region 1 (6 cells), region 5 (1 cell sharing an edge with region 1),
  # region 9 (1 cell touching region 1 by a corner only, surrounded by nodata)
  r <- terra::rast(nrows = 3, ncols = 4, xmin = 0, xmax = 80, ymin = 0, ymax = 60, crs = "EPSG:32617",
                   vals = c(1,  1,  1,  5,
                            1,  1,  1,  NA,
                            NA, NA, NA, 9))
  polygons <- terra::as.polygons(r)

  cleaned <- clean_small_polygons(polygons, min_area = prod(terra::res(r)) * 1.5)

  expect_equal(length(cleaned), 1)
  expect_equal(terra::expanse(cleaned, transform = FALSE), 7 * 400) # Region 1 + region 5; the lone cell is removed
  expect_equal(nrow(terra::geom(cleaned)[, "part", drop = FALSE] |> unique()), 1) # Single part
})
