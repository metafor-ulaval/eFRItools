test_that("metrics successfully extracted", {
  fri_polygons |>
    mutate_metrics(terra::rast(metrics), fun = "median") |>
    dplyr::pull(slope_median) |>
    mean() |>
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.743835)
})

test_that("differents crs", {
  fri_polygons |>
    sf::st_transform(4326) |>
    mutate_metrics(terra::rast(metrics), fun = "median") |>
    dplyr::pull(slope_median) |>
    mean() |>
    round(6) -> mean_slope

  expect_equal(mean_slope, 2.743835)
})
